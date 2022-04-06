package maf.cli.experiments.scv

import maf.core.Identity
import maf.util._
import maf.util.benchmarks._
import scala.concurrent.duration._
import scala.concurrent.Future
import maf.util.Reader
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme.*
import maf.language.ContractScheme.*
import maf.modular.scv.*
import maf.modular.Metric
import maf.cli.experiments.SchemeAnalyses

/** Counts the false positives in the program assuming that the program is safe (i.e. does not contain any contract violations) */
object CountFalsePositives:
    case class PrecisionResult(blames: Int, metrics: List[Metric])

    def analyses: List[((SchemeExp => ScvBigStepSemantics), String)] = List(
      (SchemeAnalyses.scvModAnalysisWithRacketFeatures, "scvModf"),
      //(SchemeAnalyses.scvModAnalysisWithRacketFeaturesWithSharedPathStore, "scvModf-sharedPs"),
      (SchemeAnalyses.scvModAnalysisWithRacketFeaturesWithPathSensitiveStore, "scvModF-sensitive")
    )

    def benchmarks: Set[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks

    def forBenchmark(name: String): Map[String, Option[PrecisionResult]] =
        val txt = Reader.loadFile(name)
        val program = SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)
        analyses.map { case (analysis, name) =>
            val anl = analysis(program)
            try
                anl.analyzeWithTimeout(Timeout.start(Duration(1, MINUTES)))
                val result = PrecisionResult(anl.summary.blames.values.flatten.toSet.size, anl.metrics)
                (name -> Some(result))
            catch case e => throw e //(name -> None)
        }.toMap

    def main(args: Array[String]): Unit =
        val results = benchmarks.map(benchmark => (benchmark, forBenchmark(benchmark)))
        val resultTable = results.foldLeft(Table.empty[String].withDefaultValue("-")) { case (table, (name, result)) =>
            result.toList.foldLeft(table) { case (table, (analysis, result)) =>
                if result.isDefined then
                    val table1 = table.add(name, s"${analysis}_blames", result.get.blames.toString)
                    result.get.metrics.foldLeft(table1)((table, metric) => table.add(name, s"${analysis}_${metric.name}", metric.result.toString))
                else table.add(name, s"${analysis}_blames", "TIMEOUT")
            }
        }

        println(resultTable)
        println(resultTable.prettyString())

        val writer = Writer.openTimeStamped(s"out/scv-precision-false-positives.csv")
        Writer.write(writer, resultTable.toCSVString(rowName = "benchmark"))
        Writer.close(writer)
