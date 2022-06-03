package maf.cli.experiments.scv

import maf.language.scheme.*
import maf.language.ContractScheme.*
import maf.modular.scv.ScvModAnalysis
import maf.modular.scv.FunctionSummaryAnalysis
import maf.util.Reader
import maf.util.Writer
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Identity
import maf.cli.experiments.SchemeAnalyses

/**
 * This program creates a dataset that consists of the number of distinct call sites for each component.
 *
 * It can be used to create a distribution of such things for a specific set of benchmarks. The distribution of function re-use indicates how well a
 * compositional analysis can improve the performance on a benchmark program
 */
object ReuseAnalysis:

    def parseProgram(txt: String): SchemeExp =
        SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)

    def mkAnalysis(program: SchemeExp): FunctionSummaryAnalysis =
        SchemeAnalyses.scvModAnalysisFunctionSummaryTopSort(program)

    def runAnalysis(path: String): List[Int] =
        val source = Reader.loadFile(path)
        val parsed = parseProgram(source)
        val analysis = mkAnalysis(parsed)
        analysis.analyze()
        analysis.callsiteSummary

    val benchmarks: List[String] =
        SchemeBenchmarkPrograms.scvNguyenBenchmarks.toList

    def main(args: Array[String]): Unit =
        val out =
            "name,metric\n" +
                benchmarks.map(benchmark => (runAnalysis(benchmark).map(r => s"$benchmark ${r.toString}").mkString("\n"))).mkString("\n")
        val outFile = Writer.openTimeStamped("out/reuseanalysis.csv")
        Writer.write(outFile, out)
        Writer.close(outFile)
        println("done")
