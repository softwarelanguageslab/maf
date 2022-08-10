package maf.language.racket

import maf.language.scheme.*
import maf.core.monad.Writer
import maf.core.Monad.MonadSyntaxOps

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

    /** Discover all the required files of a module */
    def findRequires(module: Module): List[RequireDirective] = ???

    /** Discover all provides in the module */
    def findProvides(module: Module): List[ProvideDirective] = ???

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

    /**
     * Prefix each defined identifier in the module with its module name
     *
     * @param module
     *   the module to prefix
     * @return
     *   the transformed module, as well as a mapping from the original name to the updated name
     */
    def prefix(module: Module): Writer[Module, Map[String, String]] = ???
