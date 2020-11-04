package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.Expression
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.incremental.scheme.AnalysisBuilder._
import maf.util.Reader
import maf.util.Writer._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] {

  // The maximal number of warm-up runs.
  val maxWarmupRuns = 5
  // The number of actually measured runs.
  val  measuredRuns = 30

  // The results of the evaluation.
  sealed trait Result
  case class Finished(mean: Long, stddev: Long) extends Result { override def toString: String = s"$mean±$stddev" }
  case object Timedout extends Result { override def toString: String = "∞" }
  case object NotRun   extends Result { override def toString: String = " " }
  case object Errored  extends Result { override def toString: String = "E" }

  final val initS: String = "init" // Initial run.
  final val inc1S: String = "inc1" // Incremental update.
  final val inc2S: String = "inc2" // Another incremental update (same changes, different analysis).
  final val reanS: String = "rean" // Full reanalysis.

  var results: Table[Result] = Table.empty.withDefaultValue(NotRun)

  // A single program run with the analysis.
  def onBenchmark(file: String): Unit = {
    writeln(s"Testing $file")
    val program = parse(file)

    // Warm-up.

    // Use the same timeout for the entire warm-up.
    var timeoutWarmup: Timeout.T = timeout()
    var analyses: List[Analysis] = List()
    write(s"* Warm-up standard analysis (max. $maxWarmupRuns): ")
    for (w <- 1 to maxWarmupRuns) {
      write(s"$w ")
      System.gc()
      val a = analysis(program)
      a.analyze(timeoutWarmup)
      analyses = a :: analyses
    }
    write(s"\n* Warm-up incremental analysis (max. ${analyses.length}): ")
    timeoutWarmup = timeout()
    for (a <- analyses) {
      write(s"* ")
      System.gc()
      a.updateAnalysis(timeoutWarmup) // We need an analysis that has already been (partially) run.
    }

    // Actual measurements.

    var timesInit: List[Double] = List()
    var timesInc1: List[Double] = List()
    var timesInc2: List[Double] = List()
    var timesRean: List[Double] = List()

    var inc1Timeout: Boolean = false
    var reanTimeout: Boolean = false
    var inc2Timeout: Boolean = false // For a second setup of the incremental analysis.

    write("\n* Measuring: ")
    var to: Timeout.T = Timeout.none
    for (i <- 1 to measuredRuns) {

      write(s"$i")
      val a = analysis(program)
      System.gc()
      to = timeout()
      val tb = Timer.timeOnly({a.analyze(to)})
      if (to.reached) {
        // The base line analysis timed out. Abort.
        writeln(" => Base analysis timed out.")
        results = results.add(file, initS, Timedout).add(file, inc1S, NotRun).add(file, inc2S, NotRun).add(file, reanS, NotRun)
        return
      }
      timesInit = (tb.toDouble / 1000000) :: timesInit

      val aCopy = a.deepCopy()

      write(if (inc1Timeout) "x" else "*")
      if (!inc1Timeout) {
        System.gc()
        to = timeout()
        val ti1 = Timer.timeOnly({a.updateAnalysis(to, false)}) // Do not regain precision
        if (to.reached) {
          inc1Timeout = true
        }
        timesInc1 = (ti1.toDouble / 1000000) :: timesInc1
      }

      write(if (inc2Timeout) "x" else "*")
      if (!inc2Timeout) {
        System.gc()
        to = timeout()
        val ti2 = Timer.timeOnly({aCopy.updateAnalysis(to)}) // Do regain precision
        if (to.reached) {
          inc2Timeout = true
        }
        timesInc2 = (ti2.toDouble / 1000000) :: timesInc2
      }

      write(if (reanTimeout) "x " else "* ")
      if (!reanTimeout) {
        val a = analysis(program) // Create a new analysis and set the flag to "New".
        a.version = New
        System.gc()
        to = timeout()
        val tr = Timer.timeOnly({a.analyze(to)})
        if (to.reached) {
          reanTimeout = true
        }
        timesRean = (tr.toDouble / 1000000) :: timesRean
      }
    }

    // Process statistics.
    val init = Statistics.all(timesInit)
    val inc1 = Statistics.all(timesInc1)
    val inc2 = Statistics.all(timesInc2)
    val rean = Statistics.all(timesRean)

    results = results
      .add(file, initS, Finished(scala.math.round(init.mean), scala.math.round(init.stddev)))
      .add(file, inc1S, if (inc1Timeout) Timedout else Finished(scala.math.round(inc1.mean), scala.math.round(inc1.stddev)))
      .add(file, inc2S, if (inc2Timeout) Timedout else Finished(scala.math.round(inc2.mean), scala.math.round(inc2.stddev)))
      .add(file, reanS, if (reanTimeout) Timedout else Finished(scala.math.round(rean.mean), scala.math.round(rean.stddev)))

    writeln(s"\n    => $initS: ${init.mean} - $inc1S: ${inc1.mean} - $inc2S: ${inc2.mean} - $reanS: ${rean.mean}")
  }

  def reportError(file: String): Unit = results = results.add(file, initS, Errored).add(file, inc1S, Errored).add(file, inc2S, Errored).add(file, reanS, Errored)
  def createOutput(): String = results.prettyString(columns = List(initS, inc1S, inc2S, reanS))
}

object IncrementalSchemeModFPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModF
  override def analysis(e: SchemeExp): Analysis = new IncrementalSchemeModFAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))
  val outputFile: String = s"ModF-performance.txt"
}

object IncrementalSchemeModConcPerformance extends IncrementalTime[SchemeExp] {
  override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.scam2020ModConc
  override def analysis(e: SchemeExp): Analysis = new IncrementalModConcAnalysis(e)
  override def parse(string: String): SchemeExp = CSchemeParser.parse(Reader.loadFile(string))
  override def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))
  val outputFile: String = s"ModConc-performance.txt"
}

object IncrementalSchemeModXPerformance {
  def main(args: Array[String]): Unit = {
    IncrementalSchemeModFPerformance.main(args)
    IncrementalSchemeModConcPerformance.main(args)
  }
}
