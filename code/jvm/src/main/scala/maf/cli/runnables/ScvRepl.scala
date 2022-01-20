package maf.cli.runnables

import maf.cli.experiments.SchemeAnalyses
import scala.io.StdIn.readLine
import maf.language.ContractScheme._
import maf.util.benchmarks.Timeout
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.util.Reader

object ScvRepl extends App:
    def analyse(program: String): Any =
        val exp = ContractSchemeParser.parse(program.nn)
        println(s"parsed expression $exp")
        val analysis = SchemeAnalyses.scvModAnalysisWithRacketFeatures(exp)
        analysis.analyze()
        println(analysis.summary.blames)
        println(analysis.mapStoreString())
        analysis.returnValue(analysis.initialComponent)

    def repl(): Unit =
        print(">")
        val program = readLine().trim().nn
        if program.startsWith(":f") then
            val args = program.replace(":f", "").nn.trim().nn
            val filename = args
            println(analyse(Reader.loadFile(filename)))
            repl()
        else if program != ":q" then
            println(analyse(program))
            repl()

    repl()
