package maf.cli.experiments.performance

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration.*
//import net.openhft.affinity.{AffinityLock, AffinityStrategies, AffinityThreadFactory}

trait ParallelPerformanceEvaluation(cores: Int) extends PerformanceEvaluation:
    override def addResult(name: String, benchmark: Benchmark, result: PerformanceResult, metrics: List[Metrics]): Unit =
      synchronized {
        super.addResult(name, benchmark, result, metrics)
      }

    private def measureBenchmarkFuture(
        benchmark: Benchmark,
        current: Int,
        total: Int,
        failFast: Boolean
      )(using ExecutionContext
      ): List[Future[Any]] =
      analyses.map { case (analysis, name) =>
        println(s"***** Scheduling $name on $benchmark [$current/$total] (futures) *****")
        measureAnalysis(benchmark, analysis)
          .map { case (result, metrics) =>
            println(s"Analysis $name on $benchmark done, with result $result")
            addResult(name, benchmark, result, metrics)
          }
          .recover { case cause =>
            println(s"$name of $benchmark encountered exception ${cause.getMessage}")
          }
      }

    override def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true)(using ExecutionContext) =
        val total = benchmarks.size
        val futs = (0 to total).zip(benchmarks).flatMap { case (current, b) =>
          measureBenchmarkFuture(b, current, total, failFast)
        }
        Thread.sleep(100)
        //println("\nThe assignment of CPUs is\n" + AffinityLock.dumpLocks());
        Await.result(Future.sequence(futs), Duration.Inf)

    /** timeoutfast is ignored for this type of performance benchmark */
    override def run(
        path: String = "benchOutput/performance/output.csv",
        timeoutFast: Boolean = true,
        failFast: Boolean = true
      ) =
        //import AffinityStrategies.*
        //val afinityFactory = new AffinityThreadFactory("bg", SAME_SOCKET)
        val executor = Executors.newFixedThreadPool(6).nn /*, afinityFactory).nn */
        given ctx: ExecutionContext = ExecutionContext.fromExecutor(executor)
        measureBenchmarks(timeoutFast, failFast)
        printResults()
        exportCSV(path, format _)
        exportCSV(path + "-stddev", formatStddev _)
        executor.shutdownNow
