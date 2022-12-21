package maf.language.racket

import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.core.Monad.*
import maf.language.scheme.lattices.*
import maf.core.{Address, Expression, Identifier, Identity}
import maf.util.Reader
import javax.lang.model.element.ModuleElement.ProvidesDirective
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.AScheme.ASchemeParser
import maf.lattice.HMap
import maf.lattice.AbstractSetType
import maf.util.graph.TopSort
import java.io.File

class ModuleGraph:
    /** A map from modules to their dependencies */
    private var edges: Map[Modules.Name, Set[Modules.Name]] = Map()

    /** A set of nodes */
    private var nodes: Set[Modules.Name] = Set()

    /** Adds a dependsOn edge between `modulePath` and `otherModulePath` */
    def dependsOn(moduleName: Modules.Name, otherModuleName: Modules.Name): Unit =
        edges = edges + (otherModuleName -> (edges.get(otherModuleName).getOrElse(Set()) + moduleName))

    /** Returns a topologically sorted list of the graph */
    def topsort(): List[Modules.Name] =
        TopSort.topsort(nodes.toList, edges)

    /** Register the given module in the dependency graph */
    def registerModule(moduleName: Modules.Name): Unit =
        nodes = nodes + moduleName

object Modules:
    opaque type Name = String
    opaque type Path = String

    def pathAsName(name: Path): Name = name
    def str(name: Name | Path): String = name
    def path(name: String): Path = name
    def name(nm: String): Name = nm

/**
 * Adds semantics for evaluating `RacketModule` expressions as well as resolving `require` expressions by `provide` expressions during analysis time.
 *
 * TODO[medium]: much of this can be computed without abstractly interpreting the program, this should actually be an extension of the Scheme
 * compiler.
 */
trait RacketLoaderSemantics extends SchemeSemantics, SchemeDomain, SchemeModFLocalSensitivity:
    import analysisM.*
    override def eval(exp: SchemeExp): A[Val] = exp match
        case RacketModuleLoad(module, name, idn) =>
            for
                evalMd <- eval(module)
                rmod <- merge(lattice.rmods(evalMd).map(_.lookup(name.name)).map(unit).toList)
            yield rmod

        case RacketModule(name, requireDirs, provideDirs, includes, provides, bdy, idn) =>
            for evalBdy <- eval(bdy)
            yield evalBdy

        case RacketModuleExpose(exposed, idn) =>
            for
                provided <- exposed.mapM(binding => lookupEnv(Identifier(binding._1, Identity.none)) >>= lookupSto)
                rmod = RMod[Value](exposed.map(_._2).zip(provided).toMap, None)
            yield lattice.rmod(rmod)

        // ignore require and provide statements since they are already transformed to different nodes
        case RacketRequire(_, _) => unit(lattice.nil)
        case RacketProvide(_, _) => unit(lattice.nil)

        case _ => super.eval(exp)

trait RacketLoader:
    /**
     * Should parse the given program to a valid AST
     *
     * @param prg
     *   the text of the program to parse.
     */
    def parse(prg: String): SchemeExp

    /** Runs an undefiner over the given Scheme AST */
    def undefine(exp: SchemeExp): SchemeExp

    /** Find all Scheme expressions that satisfy the given condition */
    private def find[A](e: SchemeExp)(cnd: PartialFunction[SchemeExp, A]): List[A] =
        e.subexpressions.map(_.asInstanceOf[SchemeExp]).collect(cnd)

    /**
     * Given a logical module name, returns the physical path associated with the module name
     * @param moduleName
     *   the logical name of the module
     */
    private def findPhysical(moduleName: Modules.Name): Modules.Path =
        val modulePath = Modules.str(moduleName)
        val candidates = List(modulePath, modulePath + ".rkt", modulePath + "/" + "main.rkt")
        println(s"looking up candidates $candidates")
        println(candidates.find(filename => File(filename).exists()))
        Modules.path(candidates.find(filename => File(filename).exists()).getOrElse(sys.error(s"module $moduleName not found, tried $candidates")))

    /** Parses the module on the given phyisical path to a scheme expression and discovers all its imports */
    private def parseModule(modulePath: Modules.Path): (SchemeExp, List[RequireDirective]) =
        val contents = Reader.loadFile(Modules.str(modulePath))
        // parse the file
        val parsed = parse(contents)
        // walk the parsed tree to find require statements
        val requires = find(parsed) { case r: RacketRequire =>
            r
        }.flatMap(RequireDirective.fromExp)
        (parsed, requires)

    private def resolveModule(resolvedModules: List[RacketModule], unresolvedModule: RacketModule): /* resolved */ List[RacketModule] =
        def computeLocalDefines(exp: SchemeExp): List[String] =
            exp match
                case SchemeDefineVariable(Identifier(nam, _), value, idn) =>
                    List(nam)
                case SchemeBegin(exs, _) =>
                    exs.flatMap(computeLocalDefines)
                case _ => List()

        // resolve the requires first
        val includes: List[ResolvedRequire] = unresolvedModule.requiresDirectives.flatMap(_.resolve(resolvedModules))
        // prepend defines to the body for adding the imported values to the scope
        val loadedBdy = SchemeBegin(
          includes.map(_.toDefine) ++ List(unresolvedModule.bdy),
          Identity.none
        )
        // once the requires are resolved we need to compute which members will be provided
        // for this we compute a list of locally defined members and then filter them based on the provide directives
        println(loadedBdy)
        val localDefines = computeLocalDefines(loadedBdy)
        val onlyProvides: List[SelectedProvide] = unresolvedModule.providesDirectives.flatMap(_.select(localDefines))
        // add an expose-module special form to the body so that all provided identifiers are exported
        val exposedBdy = SchemeBegin(
          loadedBdy :: List(RacketModuleExpose(onlyProvides.map((ex: SelectedProvide) => (ex.originalName -> ex.exposedName)).toMap, Identity.none)),
          Identity.none
        )
        unresolvedModule.copy(provides = onlyProvides, includes = includes, bdy = exposedBdy) :: resolvedModules

    /** Loads a Racket project and returns a list of Racket modules */
    def load(filename: Modules.Path): SchemeExp =
        val dependencyGraph = ModuleGraph()
        def loadAll(
            entry: Modules.Name,
            loadedModules: Map[Modules.Name, (SchemeExp, List[RequireDirective])]
          ): Map[Modules.Name, (SchemeExp, List[RequireDirective])] =
            dependencyGraph.registerModule(entry)
            if loadedModules.contains(entry) then
                // if we discover a module again, this means that the imports are cyclic which is not allowed
                sys.error(s"cyclic import path while trying to import $entry")
            else
                // parse the module to a valid AST
                val (entryExp, entryRequires) = parseModule(findPhysical(entry))
                // add the module to the dependency graph
                entryRequires.foreach(re => dependencyGraph.dependsOn(entry, re.moduleName))
                // recursively find the other modules and add them to the dependency graph
                entryRequires
                    .map(entry => loadAll(entry.moduleName, loadedModules))
                    .foldLeft(loadedModules + (entry -> (entryExp, entryRequires)))((entries, nextEntries) => entries ++ nextEntries)

        val modules = loadAll(Modules.pathAsName(filename), Map())
        // sort the modules topologically so that they are loaded in a correct order
        val topsorted = dependencyGraph.topsort()
        // create a racket module for each loaded module, and define its module name using a `define`
        val racketModules = topsorted.map(modulePath =>
            val (exp, requires) = modules(modulePath)
            val provideDirectives = find(exp) { case r: RacketProvide => r }
            val module = RacketModule(modulePath,
                                      requires,
                                      find(exp) { case r: RacketProvide => r }.flatMap(ProvideDirective.fromExp),
                                      List(),
                                      List(),
                                      exp,
                                      exp.idn
            )

            module
        )
        println(s"Found modules $racketModules")
        // resolve the includes and provides list in each of the modules
        // to do this we accumulate into a list of resolved racket modules
        val resolvedModules = racketModules.foldLeft(List[RacketModule]())(resolveModule)

        undefine(
          SchemeBegin(resolvedModules.reverse.map(mod => SchemeDefineVariable(Identifier(Modules.str(mod.name), Identity.none), mod, Identity.none)),
                      Identity.none
          )
        )

object ASchemeRacketLoader extends RacketLoader:
    override def parse(prg: String): SchemeExp =
        ASchemeParser.parseProgramDefines(prg)

    override def undefine(exp: SchemeExp): SchemeExp =
        SchemeUndefiner.undefine(List(exp))

/**
 * A loader that is generic over the parser, note that the parser should NOT undefine the program.
 *
 * @parser
 *   a function that takes a Scheme program and parses it into a Scheme AST
 */
class GenericRacketLoader(parser: String => SchemeExp) extends RacketLoader:
    override def parse(prg: String): SchemeExp =
        parser(prg)

    override def undefine(exp: SchemeExp): SchemeExp =
        SchemeUndefiner.undefine(List(exp))

object Test:
    def main(args: Array[String]): Unit =
        println(ASchemeRacketLoader.load(Modules.path("test.rkt")))
