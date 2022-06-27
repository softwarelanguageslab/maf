package maf.cli.runnables

import scala.io.StdIn
import maf.language.AScheme.ASchemeParser
import maf.language.scheme.SchemeBegin
import maf.core.Identity
import maf.modular.scheme.modactor.SimpleSchemeModActorAnalysis
import maf.util.Reader
import maf.language.AScheme.interpreter.CPSASchemeInterpreter
import maf.language.scheme.*
import maf.util.benchmarks.*

object ASchemeRepl:
    private var concrete: Boolean = true

    private def run(program: List[SchemeExp]): Unit =
        if concrete then
            val interpreter = CPSASchemeInterpreter()
            println(interpreter.run(SchemeBegin(program, Identity.none), Timeout.none))
        else
            val analysis = new SimpleSchemeModActorAnalysis(SchemeBegin(program, Identity.none))
            analysis.analyze()
            println(analysis.storeString())

    def main(args: Array[String]): Unit =
        print("> ")
        val input = StdIn.readLine().trim().nn
        if input != ":q" then
            val program = if !input.startsWith(":f") then input else Reader.loadFile(input.split(' ')(1).nn)
            val parsed = ASchemeParser.parse(program)
            run(parsed)
            main(args)
        else ()
