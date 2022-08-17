package maf.language.racket

import maf.language.scheme.*
import maf.util.MonoidImplicits
import maf.core.monad.*
import maf.core.monad.MonadLift.*
import maf.core.Monad
import maf.core.Monad.*
import maf.core.Identifier
import maf.language.sexp.Value

/**
 * This class provides several methods to produce a single Racket file from a collection of Racket files based on Racket's module system.
 *
 * @param modulesByFilename
 *   a map from filenames to module expressions
 */
class ModuleConcatinator(modulesByFilename: Map[String, SchemeExp]):
    type DAG = Unit
    type Node = Unit
    type Module = List[SchemeExp]
    type Env = Map[String, String]

    private def parseRequireDirective(directive: SchemeExp): RequireDirective = directive match
        case SchemeValue(Value.String(s), _) => RequireDirective.RequireFile(s)
        case SchemeFuncall(SchemeVar(Identifier("prefix-in", _)), List(directive, SchemeVar(Identifier(prefix, _))), _) =>
            RequireDirective.Prefix(parseRequireDirective(directive), prefix)
        case SchemeFuncall(SchemeVar(Identifier("rename-in", _)), directive :: renames, _) =>
            RequireDirective.Rename(
              parseRequireDirective(directive),
              renames.map { case SchemeFuncall(SchemeVar(Identifier(original, _)), List(SchemeVar(Identifier(renamed, _))), _) =>
                  (original -> renamed)
              }.toMap
            )
        case SchemeFuncall(SchemeVar(Identifier("except-in", _)), directive :: exceptList, _) =>
            RequireDirective.Except(parseRequireDirective(directive),
                                    exceptList.map { case SchemeVar(Identifier(name, _)) =>
                                        name
                                    }
            )
        case _ => throw new Exception(s"Given require-spec is not supported $directive")

    private def findRequireSingle(exp: SchemeExp): List[RequireDirective] = exp match
        case SchemeFuncall(SchemeVar(Identifier("require", _)), directives, _) =>
            directives.map(parseRequireDirective)
        case _ => List()

    /** Discover all the required files of a module */
    def findRequires(module: Module): List[RequireDirective] = module match
        case List()  => List()
        case x :: xs => findRequireSingle(x) ++ findRequires(xs)

    private def parseProvideDirective(directive: SchemeExp): ProvideDirective = directive match
        case SchemeFuncall(Identifier("all-defined-out", _), List(), _) =>
            ProvideDirective.AllDefinedOut
        case SchemeFuncall(Identifier("except-out", _), directive :: exceptList, _) =>
            ProvideDirective.Except(parseProvideDirective(directive),
                                    exceptList.map { case SchemeVar(Identifier(name, _)) =>
                                        name
                                    }
            )
        case SchemeVar(Identifier(name, _)) => ProvideDirective.IdentifierOut(name)

    private def findProvidesSingle(e: SchemeExp): List[ProvideDirective] = e match
        case SchemeFuncall(Identifier("provide", _), directives, _) =>
            directives.map(parseProvideDirective)
        case _ => List()

    /** Discover all provides in the module */
    def findProvides(module: Module): List[ProvideDirective] = module match
        case List()  => List()
        case x :: xs => findProvidesSingle(x) ++ findProvides(xs)

    /**
     * Find an identifier from import list of the modules
     *
     * @param id
     *   the identifier we are lookign for
     * @param importList
     *   the list of imports made by the file
     * @param modules
     *   a mapping from module names, to their provide directives and translations
     */
    def findRequirement(id: String, importList: List[RequireDirective], modules: Map[String, (ProvideDirective, Map[String, String])]): String = ???

    /** Build a DAG from the required modules so that we know which definitions to place first */
    def buildDAG(requirements: Map[String, List[String]]): DAG = ???

    /**
     * Do a topological sorting of the DAG
     *
     * @param dag
     *   the dag to toplogically sort
     * @return
     *   a list of filenames, in order that they should be defined
     */
    def topSort(dag: DAG): List[String] = ???

    type W[A] = ReaderT[[A] =>> Writer[List[(String, String)], A], Env, A]

    private def recordVar(old: String, nww: String): W[String] =
        ReaderT.lift(Writer.tell(List((old, nww)))) >>> Monad[W].unit(nww)

    private def isExported(name: String): W[Boolean] = ???
    private def prefixModuleName(name: String): W[(String, String)] = ???
    private def prefixId[T](old: T, name: String)(f: String => T): W[T] =
        // Only rename if the environment contains a rename
        mIf(isExported(name)) /* then */ {
            // prefix the identifier with the current module name
            prefixModuleName(name).flatMap(recordVar).map(f)
        } /* else */ {
            // return the definition without renamign
            Monad[W].unit(old)
        }

    private def addToEnv(old: String, nww: String)(env: Env): Env = ???

    private def scoped[T](old: String, nww: String)(f: W[T]): W[T] =
        ReaderT.local(addToEnv(old, nww))(f)

    private def prefixSingle(topLevel: Boolean)(e: SchemeExp): W[SchemeExp] = e match
        case dfv @ SchemeDefineVariable(id, value, idn) if topLevel =>
            for
                prefixed <- prefixId(dfv, id.name)(nam => dfv.copy(name = Identifier(nam, id.idn)))
                valuePrefixed <- prefixSingle(false)(value)
            yield prefixed.copy(value = valuePrefixed)

        case dfv @ SchemeDefineVariable(id, value, idn) =>
            scoped(id.name, id.name) { prefixSingle(false)(value).map(v => dfv.copy(value = v)) }

        case _ => ???

    /**
     * Prefix each defined identifier in the module with its module name
     *
     * @param module
     *   the module to prefix
     * @return
     *   the transformed module, as well as a mapping from the original name to the updated name
     */
    def prefix(module: Module): W[Module] =
        module.mapM(prefixSingle(true))
