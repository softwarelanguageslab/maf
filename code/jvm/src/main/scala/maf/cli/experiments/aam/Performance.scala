package maf.cli.experiments.aam

import maf.cli.experiments.performance.*
import maf.aam.*
import maf.language.scheme.*
import maf.modular.Metric
import maf.language.ContractScheme.*
import maf.core.*
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.benchmarks.*
import maf.modular.*
import maf.util.graph.*
import maf.cli.experiments.SchemeAnalyses
import maf.aam.scheme.AAMPeformanceMetrics

/** Compare the performance of AAM with a ModF style analysis */
trait AAMPerformanceComparison extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]

    protected def wrap(f: SchemeExp => AAMPeformanceMetrics[SchemeExp]): SchemeExp => Analysis = (exp) => f(exp)
    protected def wrapModF(f: SchemeExp => ModAnalysis[SchemeExp]): SchemeExp => Analysis = (exp) => f(exp)

object AAMModFPerformanceComparison extends AAMPerformanceComparison:
    override def maxWarmupRuns = 0
    override def analysisRuns = 2
    def _benchmarks = Set("test/R5RS/various/blur.scm")
    def __benchmarks = SchemeBenchmarkPrograms.various -- Set(
      "test/R5RS/various/loop2.scm", // weirdly seems to be stuck for classic AAM
      "test/R5RS/various/grid.scm", // timeout even with function boundaries
      "test/R5RS/various/pico.scm", // weird errors about continuations
      "test/R5RS/various/regex.scm", // time-out? why?
      "test/R5RS/various/mceval.scm"
    )

    def ___benchmarks: Set[String] = SchemeBenchmarkPrograms.jss2021
    def benchmarks: Set[String] = Set(
      "test/R5RS/WeiChenRompf2019/toplas98/boyer.scm",
    )

    def analyses: List[(SchemeExp => Analysis, String)] =
      List(
        //(wrap(AAMAnalyses.aamBase), "aamBase"),
        //(wrap(AAMAnalyses.aamConf1), "aamConf1"),
        //(wrap(AAMAnalyses.aamConf2), "aamConf2"),
        //(wrap(AAMAnalyses.aamConf3), "aamConf3"),
        //(wrap(AAMAnalyses.aamConf4), "aamConf4"),
        (wrap(AAMAnalyses.aamConf6), "aamConf6"),
        (wrapModF(SchemeAnalyses.kCFAAnalysis(_, 1)), "1cfaModf")
      )
    def main(args: Array[String]): Unit =
      run(timeoutFast = false)

object ScvPerformanceComparison extends AAMPerformanceComparison:
    override def parseProgram(txt: String): SchemeExp =
        val result = SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)
        println(s"input program ${result.prettyString(0)}")
        result

    //def benchmarks = SchemeBenchmarkPrograms.scvNguyenBenchmarks
    def benchmarks = SchemeBenchmarkPrograms.scvNguyenBenchmarks

    def analyses: List[(SchemeExp => Analysis, String)] =
      List(
        //(wrap(AAMAnalyses.scvAAMFnCallBoundaries), "scvAAMFfn"),
        (wrapModF(SchemeAnalyses.scvModAnalysisWithRacketFeatures), "scvModf")
      )

    def main(args: Array[String]): Unit =
      run(timeoutFast = false)

object ModFSingleBenchmark extends AAMPerformanceComparison with ParallelPerformanceEvaluation(6):
    def benchmarks = SchemeBenchmarkPrograms.various

    def analyses: List[(SchemeExp => Analysis, String)] =
      List((wrapModF(SchemeAnalyses.kCFAAnalysis(_, 0)), "0cfaModf"))

    def main(args: Array[String]): Unit =
        Thread.sleep(20000)
        run(timeoutFast = false)
