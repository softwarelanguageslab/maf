package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.util._
import maf.util.benchmarks.Timeout
import maf.bench.scheme.SchemeBenchmarkPrograms

import scala.concurrent.duration._

object DailyPrecisionBenchmarks
    extends AnalysisComparison[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:
    // analyses to compare
    def baseAnalysis(prg: SchemeExp): Analysis =
      SchemeAnalyses.contextInsensitiveAnalysis(prg)
    def otherAnalyses() = List(
      (SchemeAnalyses.callSiteContextSensitiveAnalysis, "1CS")
      //(SchemeAnalyses.adaptiveAnalysisPolicy3(_, 5), "adaptive-policy-3")
    )

    // benchmarks to run
    def benchmarks = SchemeBenchmarkPrograms.gabriel

    // timeout configuration
    override def analysisTimeout() = Timeout.start(Duration(15, MINUTES)) //timeout for (non-base) analyses
    override def concreteTimeout() = Timeout.start(Duration(5, MINUTES))

    // entry point
    def main(args: Array[String]) =
        benchmarks.foreach(runBenchmark)
        println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
        Writer.setDefaultWriter(Writer.open("benchOutput/precision/daily-precision-benchmarks.csv"))
        Writer.write(results.toCSVString(format = _.map(_.toString()).getOrElse("TIMEOUT"), rowName = "benchmark"))
        Writer.closeDefaultWriter()
