package maf.cli.experiments.performance

import maf.language.scheme._
import maf.language.CScheme._
import maf.modular.ModAnalysis
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

trait AnalysisIsFinished[T]:
    def isFinished(analysis: T): Boolean
    def doAnalyzeWithTimeout(analysis: T, timeout: Timeout.T): Any

    extension (analysis: T)
        def finished: Boolean = isFinished(analysis)
        def analyzeWithTimeout(timeout: Timeout.T): Any = doAnalyzeWithTimeout(analysis, timeout)

object AnalysisIsFinished:
    given AnalysisIsFinished[ModAnalysis[SchemeExp]] with
        def isFinished(analysis: ModAnalysis[SchemeExp]): Boolean = analysis.finished
        def doAnalyzeWithTimeout(analysis: ModAnalysis[SchemeExp], timeout: Timeout.T): Any =
          analysis.analyzeWithTimeout(timeout)

    given AnalysisIsFinished[AAMAnalysis] with
        def isFinished(analysis: AAMAnalysis): Boolean = analysis.finished
        def doAnalyzeWithTimeout(analysis: AAMAnalysis, timeout: Timeout.T): Any =
          analysis.analyzeWithTimeout(timeout)

trait PerformanceEvaluation:
    type Analysis

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
      )(using af: AnalysisIsFinished[Analysis],
        ex: ExecutionContext
      ): Future[PerformanceResult] =
        def run(): PerformanceResult =
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
            for i <- 1 to analysisRuns do
                print(s"$i ")
                val a = analysis(program)
                System.gc()
                val t = Timer.timeOnly(a.analyzeWithTimeout(analysisTime))
                if a.finished then times = (t.toDouble / 1000000) :: times
                else return TimedOut // immediately return
            print("\n")
            // Compute, print and return the results
            val result = Statistics.all(times)
            println(times.mkString("[", ",", "]"))
            println(result)
            Completed(result)
        Future { run() }

    // Runs the evaluation
    def measureBenchmark(
        benchmark: Benchmark,
        current: Int,
        total: Int,
        timeoutFast: Boolean,
        failFast: Boolean
      )(using AnalysisIsFinished[Analysis],
        ExecutionContext
      ): Unit =
      analyses.foreach { case (analysis, name) =>
        try
            println(s"***** Running $name on $benchmark [$current/$total] *****")
            val result = Await.result(measureAnalysis(benchmark, analysis), Duration.Inf)
            results = results.add(benchmark, name, result)
            result match
                case TimedOut if timeoutFast => return
                case _                       => ()
        catch
            case e: Exception =>
              println(s"Encountered an exception: ${e.getMessage}")
              if failFast then return
            case e: VirtualMachineError =>
              System.gc()
              println(s"Running $benchmark resulted in an error: ${e.getMessage}")
              if failFast then return
      }

    def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true)(using AnalysisIsFinished[Analysis], ExecutionContext) =
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
      )(using AnalysisIsFinished[Analysis]
      ) =
        given ExecutionContext with
            def execute(runnable: Runnable): Unit = runnable.run
            def reportFailure(cause: Throwable): Unit = throw cause
        measureBenchmarks(timeoutFast, failFast)
        printResults()
        exportCSV(path, format _)
        exportCSV(path + "-stddev", formatStddev _)
