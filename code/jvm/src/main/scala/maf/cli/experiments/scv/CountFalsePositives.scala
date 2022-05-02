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
import java.util.concurrent.TimeoutException

/** Counts the false positives in the program assuming that the program is safe (i.e. does not contain any contract violations) */
object CountFalsePositives:
    case class PrecisionResult(blames: Int, metrics: List[Metric])

    enum AnalysisResult[+R]:
        // the analysis timed out
        case TimedOut
        // we got a result from the analysis
        case Result(result: R)
        // the analysis resulted in an uncaught exception
        case Error

    def analyses: List[((SchemeExp => ScvBigStepSemantics), String)] = List(
      (SchemeAnalyses.scvModAnalysisWithRacketFeatures, "scvModf"),
      //(SchemeAnalyses.scvModAnalysisWithRacketFeaturesWithPathSensitiveStore, "scvModF-sensitive"),
      //(SchemeAnalyses.scvModAnalysisWithRacketFeaturesWithIncomingFlow, "scvModF-incoming-flow"),
      (SchemeAnalyses.scvModAnalysisRktFsR, "scvModF-rkt-fs-r")
    )

    def benchmarks: Set[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks

    def forBenchmark(name: String): Map[String, (AnalysisResult[PrecisionResult], ScvBigStepSemantics)] =
        import AnalysisResult.*
        val txt = Reader.loadFile(name)
        val program = SchemeBegin(ContractSchemeMutableVarBoxer.transform(List(ContractSchemeParser.parse(txt))), Identity.none)
        analyses.map { case (analysis, name) =>
            val anl = analysis(program)
            try
                anl.analyzeWithTimeout(Timeout.start(Duration(1, MINUTES)))
                val result = PrecisionResult(anl.summary.blames.values.flatten.toSet.size, anl.metrics)
                (name -> (Result(result), anl))
            catch
                case _: TimeoutException => (name -> (TimedOut, anl))
                case e                   => (name -> (Error, anl))
        }.toMap

    def main(args: Array[String]): Unit =
        val results = benchmarks.map(benchmark => (benchmark, forBenchmark(benchmark)))
        val resultTable = results.foldLeft(Table.empty[String].withDefaultValue("-")) { case (table, (name, result)) =>
            result.toList.foldLeft(table) { case (table, (analysis, (result, anl))) =>
                val metrics = List(
                  anl.NumberOfComponents,
                  anl.NumberOfIntra,
                  anl.Z3Time,
                  anl.Z3InterpreterTime,
                  anl.SATExec,
                  anl.SATCacheHit,
                  anl.AppArr,
                  anl.AppFlat,
                  anl.EvalCheck,
                  anl.ImplicitContract
                )

                import AnalysisResult.*
                val (addedMetrics, resTable) = result match
                    case Result(res) =>
                        val table1 = table.add(name, s"${analysis}_blames", res.blames.toString)
                        val table2 =
                            res.metrics.foldLeft(table1)((table, metric) => table.add(name, s"${analysis}_${metric.name}", metric.result.toString))
                        (res.metrics.map(_.name), table2)
                    case TimedOut =>
                        (List(), table.add(name, s"${analysis}_blames", "TIMEOUT"))

                    case Error =>
                        (List(), table.add(name, s"${analysis}_blames", "ERROR"))

                val remainingMetrics = metrics.map(_.name).toSet -- addedMetrics.toSet

                remainingMetrics.foldLeft(resTable)((table, metric) => table.add(name, s"${analysis}_${metric}", "-"))

            }
        }

        println(resultTable)
        println(resultTable.prettyString())

        val writer = Writer.openTimeStamped(s"out/scv-precision-false-positives.csv")
        Writer.write(writer, resultTable.toCSVString(rowName = "benchmark"))
        Writer.close(writer)
