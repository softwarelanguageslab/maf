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

class ModuleGraph:

    /** Adds a dependsOn edge between `modulePath` and `otherModulePath` */
    def dependsOn(modulePath: String, otherModulePath: String): Unit = ???

    /** Returns a topologically sorted list of the graph */
    def topsort(): List[String] = ???

    /** Register the given module in the dependency graph */
    def registerModule(modulePath: String): Unit = ???

/**
 * Adds semantics for evaluating `RacketModule` expressions as well as resolving `require` expressions by `provide` expressions during analysis time.
 *
 * TODO[medium]: much of this can be computed without abstractly interpreting the program, this should actually be an extension of the Scheme
 * compiler.
 */
trait RacketLoaderSemantics extends SchemeSemantics, RacketDomain, SchemeModFLocalSensitivity:
    import analysisM.*
    override def eval(exp: SchemeExp): A[Val] = exp match
        case RacketModuleLoad(module, name, idn) =>
            for
                evalMd <- eval(module)
                rmod <- merge(lattice.rmods(evalMd).map(_.lookup(name.name)).map(unit))
            yield rmod
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
    def undefine(exp: SchemeExp): SchemeExp = ???

    /** Find all Scheme expressions that satisfy the given condition */
    private def find[A](e: SchemeExp)(cnd: PartialFunction[SchemeExp, A]): List[A] =
        e.subexpressions.map(_.asInstanceOf[SchemeExp]).collect(cnd)

    /**
     * Given a logical module name, returns the physical path associated with the module name
     * @param moduleName
     *   the logical name of the module
     */
    private def findPhysical(moduleName: String): String = ???

    /** Parses the module on the given phyisical path to a scheme expression and discovers all its imports */
    private def parseModule(modulePath: String): (SchemeExp, List[RequireDirective]) =
        val contents = Reader.loadFile(modulePath)
        // parse the file
        val parsed = parse(contents)
        // walk the parsed tree to find require statements
        val requires = find(parsed) { case r: RacketRequire =>
            r
        }.map(RequireDirective.fromExp)
        (parsed, requires)

    /** Loads a Racket project and returns a list of Racket modules */
    def load(filename: String): SchemeExp =
        val dependencyGraph = ModuleGraph()
        def loadAll(
            entry: String,
            loadedModules: Map[String, (SchemeExp, List[RequireDirective])]
          ): Map[String, (SchemeExp, List[RequireDirective])] =
            dependencyGraph.registerModule(entry)
            if loadedModules.contains(entry) then
                // if we discover a module again, this means that the imports are cyclic which is not allowed
                sys.error(s"cyclic import path while trying to import $entry")
            else
                // parse the module to a valid AST
                val (entryExp, entryRequires) = parseModule(filename)
                // add the module to the dependency graph
                entryRequires.foreach(entry => dependencyGraph.dependsOn(filename, findPhysical(entry.moduleName)))
                // recursively find the other modules and add them to the dependency graph
                entryRequires
                    .map(entry => loadAll(findPhysical(entry.moduleName), loadedModules))
                    .foldLeft(loadedModules + (filename -> (entryExp, entryRequires)))((entries, nextEntries) => entries ++ nextEntries)

        val modules = loadAll(filename, Map())
        // sort the modules topologically so that they are loaded in a correct order
        val topsorted = dependencyGraph.topsort()
        // create a racket module for each loaded module, and define its module name using a `define`
        val racketModules = topsorted.map(modulePath =>
            val (exp, requires) = modules(modulePath)
            val provideDirectives = find(exp) { case r: RacketProvide => r }
            val module = RacketModule(requires, find(exp) { case r: RacketProvide => r }.map(ProvideDirective.fromExp), List(), List(), exp, exp.idn)
            SchemeDefineVariable(Identifier(modulePath, exp.idn), module, exp.idn)
        )

        undefine(SchemeBegin(racketModules, Identity.none))

object ASchemeRacketLoader extends RacketLoader:
    override def parse(prg: String): SchemeExp =
        ASchemeParser.parseProgramDefines(prg)
