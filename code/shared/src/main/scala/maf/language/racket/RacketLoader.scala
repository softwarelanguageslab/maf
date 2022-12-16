package maf.language.racket

import maf.language.scheme.*
import maf.core.Expression
import maf.util.Reader

trait RacketLoader:
    def parse(prg: String): SchemeExp

    private def find[A](e: SchemeExp)(cnd: PartialFunction[SchemeExp, A]): List[A] =
        e.subexpressions.map(_.asInstanceOf[SchemeExp]).collect(cnd)

    private def findPhysical(moduleName: String): String = ???

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
    def load(filename: String): RacketModule =
        //def loadAll(entry: String, loadedModules: Map[String, (SchemeExp, List[RequireDirective])]): Map[(String, (SchemeExp, List[RequireDirective]))] =
        //    if loadedModules.contains(entry) then loadedModules
        //    else
        //        val (entryExp, entryRequires) = parseModule(filename)
        //        // entryRequires.map(entry => loadAll(entry, loadedModules))
        //        ???

        // loaded modules: modulePath => (SchemeExp, List[RequireDirective])
        var loadedModules: Map[String, (SchemeExp, List[RequireDirective])] = Map()
        // try to load the entry module

        ???
