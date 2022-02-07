package maf.cli.experiments.performance

import maf.language.scheme._
import maf.language.CScheme._
import maf.modular.{AnalysisEntry, Metric, ModAnalysis}
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._
import scala.concurrent.Future
import maf.aam.AAMAnalysis
import scala.concurrent.ExecutionContext
import scala.concurrent.Await

// A variable that holds the results
sealed trait PerformanceResult
case class Completed(results: Statistics.Stats) extends PerformanceResult
case object TimedOut extends PerformanceResult
case object NoData extends PerformanceResult

// A variable that holds additional metrics
case class Metrics(name: String, results: Statistics.Stats)

trait PerformanceEvaluation:
    type Analysis <: AnalysisEntry[SchemeExp]

    // Configuring the warm-up
    def maxWarmupRuns = 10 // maximum number of warm-up runs
    def maxWarmupTime = Timeout.start(Duration(1, MINUTES)) // maximum time to spend on warm-up *in total* (i.e., for all runs)

    // Configuring the analysis runs
    def analysisRuns = 20 // number of analysis runs
    def analysisTime = Timeout.start(Duration(5, MINUTES)) // maximum time to spend *on a single analysis run*

    // The list of benchmarks used for the evaluation
    type Benchmark = String
    def benchmarks: Iterable[Benchmark]

    // The analyses that are evaluated (and their names)
    def analyses: List[(SchemeExp => Analysis, String)]

    var results = Table.empty[PerformanceResult].withDefaultValue(NoData)

    def format(res: PerformanceResult): String = res match
        case Completed(results) => scala.math.round(results.mean).toString
        case TimedOut           => "TIMEOUT"
        case NoData             => "_"
    def formatStddev(res: PerformanceResult): String = res match
        case Completed(results) => scala.math.round(results.stddev).toString
        case TimedOut           => "TIMEOUT"
        case NoData             => "_"

    def parseProgram(txt: String): SchemeExp = CSchemeParser.parseProgram(txt)

    // Runs a single analysis multiple times and returns the mean timing (in milliseconds)
    def measureAnalysis(
        file: String,
        analysis: SchemeExp => Analysis
      )(using ex: ExecutionContext
      ): Future[(PerformanceResult, List[Metrics])] =
        def run(): (PerformanceResult, List[Metrics]) =
            // Parse the program
            val program = parseProgram(Reader.loadFile(file))
            // Warm-up
            print(s"* WARM-UP ($maxWarmupRuns) - ")
            val warmupTimeout = maxWarmupTime
            for i <- 1 to maxWarmupRuns do
                print(s"$i ")
                System.gc() // It never hurts (hopefully, because it may cause GC errors...)
                analysis(program).analyzeWithTimeout(warmupTimeout)
            print("\n")
            // Actual timing
            print(s"* RUNS ($analysisRuns) - ")
            var times: List[Double] = List()
            var metrics: Map[String, List[Double]] = Map()

            for i <- 1 to analysisRuns do
                print(s"$i ")
                val a = analysis(program)
                System.gc()
                val t = Timer.timeOnly(a.analyzeWithTimeout(analysisTime))
                if a.finished then
                    val analysisMetrics = a.metrics
                    metrics = analysisMetrics.foldLeft(metrics)((metrics, metric) =>
                      metrics + (metric.name -> (metric.result :: metrics.get(metric.name).getOrElse(List())))
                    )

                    times = (t.toDouble / 1000000) :: times
                else return (TimedOut, List()) // immediately return
            print("\n")
            // Compute, print and return the results
            val result = Statistics.all(times)
            println(times.mkString("[", ",", "]"))
            println(result)

            // Also compute statistics about additional metrics reported by the analysis
            val resultMetrics = metrics.map { case (name, metrics) =>
              Metrics(name, Statistics.all(metrics))
            }.toList

            (Completed(result), resultMetrics)
        Future { run() }

    protected def addResult(name: String, benchmark: Benchmark, result: PerformanceResult, metrics: List[Metrics]): Unit =
        results = results.add(benchmark, name, result)
        metrics.foreach(metric => results = results.add(benchmark, s"$name (${metric.name})", Completed(metric.results)))

    // Runs the evaluation
    def measureBenchmark(
        benchmark: Benchmark,
        current: Int,
        total: Int,
        timeoutFast: Boolean,
        failFast: Boolean
      )(using ExecutionContext
      ): Unit =
      analyses.foreach { case (analysis, name) =>
        try
            println(s"***** Running $name on $benchmark [$current/$total] *****")
            val (result, metrics) = Await.result(measureAnalysis(benchmark, analysis), Duration.Inf)
            addResult(name, benchmark, result, metrics)
            result match
                case TimedOut if timeoutFast => return
                case _                       => ()
        catch
            case e: Exception =>
              println(s"Encountered an exception: ${e.getMessage}")
              e.printStackTrace()
              if failFast then return
            case e: VirtualMachineError =>
              System.gc()
              println(s"Running $benchmark resulted in an error: ${e.getMessage}")
              e.printStackTrace()
              if failFast then return
      }

    def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true)(using ExecutionContext) =
        var current = 0
        val total = benchmarks.size
        benchmarks.foreach { b =>
            current += 1
            measureBenchmark(b, current, total, timeoutFast, failFast)
        }

    def printResults() =
      println(results.prettyString(format = format))
    def exportCSV(
        path: String,
        format: PerformanceResult => String,
        timestamped: Boolean = true
      ) =
        val hdl = if timestamped then Writer.openTimeStamped(path) else Writer.open(path)
        val csv = results.toCSVString(format = format)
        Writer.write(hdl, csv)
        Writer.close(hdl)

    def run(
        path: String = "benchOutput/performance/output.csv",
        timeoutFast: Boolean = true,
        failFast: Boolean = true
      ) =
        given ExecutionContext with
            def execute(runnable: Runnable): Unit = runnable.run
            def reportFailure(cause: Throwable): Unit = throw cause
        measureBenchmarks(timeoutFast, failFast)
        printResults()
        exportCSV(path, format _)
        exportCSV(path + "-stddev", formatStddev _)
