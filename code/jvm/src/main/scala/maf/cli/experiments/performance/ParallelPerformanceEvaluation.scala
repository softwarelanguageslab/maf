package maf.cli.experiments.performance

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration.*

trait ParallelPerformanceEvaluation(cores: Int) extends PerformanceEvaluation:
    private def addResult(name: String, benchmark: Benchmark, result: PerformanceResult): Unit =
      this.synchronized {
        results = results.add(benchmark, name, result)
      }

    private def measureBenchmarkFuture(
        benchmark: Benchmark,
        current: Int,
        total: Int,
        failFast: Boolean
      )(using AnalysisIsFinished[Analysis],
        ExecutionContext
      ): List[Future[Any]] =
      analyses.map { case (analysis, name) =>
        println(s"***** Scheduling $name on $benchmark [$current/$total] (futures) *****")
        measureAnalysis(benchmark, analysis)
          .map(result =>
              println(s"Analysis $name on $benchmark done, with result $result")
              addResult(name, benchmark, result)
          )
          .recover { case cause =>
            println(s"$name of $benchmark encountered exception ${cause.getMessage}")
          }
      }

    override def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true)(using AnalysisIsFinished[Analysis], ExecutionContext) =
        val total = benchmarks.size
        val futs = (0 to total).zip(benchmarks).flatMap { case (current, b) =>
          measureBenchmarkFuture(b, current, total, failFast)
        }
        Await.result(Future.sequence(futs), Duration.Inf)

    /** timeoutfast is ignored for this type of performance benchmark */
    override def run(
        path: String = "benchOutput/performance/output.csv",
        timeoutFast: Boolean = true,
        failFast: Boolean = true
      )(using AnalysisIsFinished[Analysis]
      ) =
        val executor = Executors.newFixedThreadPool(cores).nn
        given ctx: ExecutionContext = ExecutionContext.fromExecutor(executor)
        measureBenchmarks(timeoutFast, failFast)
        printResults()
        exportCSV(path, format _)
        exportCSV(path + "-stddev", formatStddev _)
        executor.shutdownNow
