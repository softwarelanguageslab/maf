package maf.cli.experiments.performance

import maf.language.scheme._
import maf.language.CScheme._
import maf.modular.ModAnalysis
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait PerformanceEvaluation {

  type Analysis = ModAnalysis[SchemeExp]

  // Configuring the warm-up
  def maxWarmupRuns = 10                                    // maximum number of warm-up runs
  def maxWarmupTime = Timeout.start(Duration(1, MINUTES))   // maximum time to spend on warm-up *in total* (i.e., for all runs)

  // Configuring the analysis runs
  def analysisRuns = 20                                     // number of analysis runs
  def analysisTime = Timeout.start(Duration(60, MINUTES))   // maximum time to spend *on a single analysis run*

  // The list of benchmarks used for the evaluation
  type Benchmark = String
  def benchmarks: Iterable[Benchmark]

  // The analyses that are evaluated (and their names)
  def analyses: List[(SchemeExp => Analysis, String)]

  // A variable that holds the results
  sealed trait PerformanceResult
  case class Completed(results: Statistics.Stats) extends PerformanceResult
  case object TimedOut extends PerformanceResult
  case object NoData extends PerformanceResult

  var results = Table.empty[PerformanceResult].withDefaultValue(NoData)

  def format(res: PerformanceResult): String = res match {
    case Completed(results) => scala.math.round(results.mean).toString
    case TimedOut           => "TIMEOUT"
    case NoData             => "_"
  }

  // Runs a single analysis multiple times and returns the mean timing (in milliseconds)
  def measureAnalysis(file: String, analysis: SchemeExp => Analysis): PerformanceResult = {
    // Parse the program
    val program = CSchemeParser.parse(Reader.loadFile(file))
    // Warm-up
    print(s"* WARM-UP ($maxWarmupRuns) - ")
    val warmupTimeout = maxWarmupTime
    for (i <- 1 to maxWarmupRuns) {
      print(s"$i ")
      System.gc() // It never hurts (hopefully, because it may cause GC errors...)
      analysis(program).analyze(warmupTimeout)
    }
    print("\n")
    // Actual timing
    print(s"* RUNS ($analysisRuns) - ")
    var times: List[Double] = List()
    for (i <- 1 to analysisRuns) {
      print(s"$i ")
      val a = analysis(program)
      System.gc()
      val t = Timer.timeOnly { a.analyze(analysisTime) }
      if (a.finished()) {
        times = (t.toDouble / 1000000) :: times
      } else {
        return TimedOut  // immediately return
      }
    }
    print("\n")
    // Compute, print and return the results
    val result = Statistics.all(times)
    println(times.mkString("[",",","]"))
    println(result)
    Completed(result)
  }

  // Runs the evaluation
  def measureBenchmark(benchmark: Benchmark, timeoutFast: Boolean = true, failFast: Boolean = true): Unit =
    analyses.foreach { case (analysis, name) =>
      try {
        println(s"***** Running $name on $benchmark *****")
        val result = measureAnalysis(benchmark, analysis)
        results = results.add(benchmark, name, result)
        result match {
          case TimedOut if timeoutFast => return
          case _ => () 
        }
      } catch {
        case e: Exception =>
          println(s"Encountered an exception: ${e.getMessage}")
          if (failFast) return
        case e: VirtualMachineError => 
          System.gc()
          println(s"Running $benchmark resulted in an error: ${e.getMessage}")
          if (failFast) return
      }
    }

  def measureBenchmarks(timeoutFast: Boolean = true, failFast: Boolean = true) = 
    benchmarks.foreach(b => measureBenchmark(b, timeoutFast, failFast))

  def printResults() = 
    println(results.prettyString(format = format))
  def exportCSV(path: String) = {
    val hdl = Writer.openTimeStamped(path)
    val csv = results.toCSVString(format = format)
    Writer.write(hdl, csv)
    Writer.close(hdl)
  }

  def run(path: String = "benchOutput/performance/output.csv", timeoutFast: Boolean = true, failFast: Boolean = true) = {
    measureBenchmarks(timeoutFast, failFast)
    printResults()
    exportCSV(path)
  }
}
