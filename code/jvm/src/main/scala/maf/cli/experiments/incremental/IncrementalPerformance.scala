package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.Expression
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations._
import maf.util.Reader
import maf.util.benchmarks._

import scala.concurrent.duration._

// The results of the evaluation.
sealed trait Result

case class Value(v: Long) extends Result:
    override def toString: String = v.toString

case object Timedout extends Result:
    override def toString: String = "∞"

case object NotRun extends Result:
    override def toString: String = " "

case object Errored extends Result:
    override def toString: String = "E"

trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] with TableOutput[Result]:

    type Analysis = IncrementalModAnalysis[E] with IncrementalGlobalStore[E]

    // The maximal number of warm-up runs.
    val maxWarmupRuns = 5
    // The number of actually measured runs.
    val measuredRuns = 30

    val timeS: String = "ms" // Mean of measured times
    val stdS: String = "SD" // Standard deviation of mean
    val propertiesS: List[String] = List(timeS, stdS)
    var results: Table[Result] = Table.empty.withDefaultValue(NotRun)
    val error: Result = Errored

    def runOneTime(analysis: Analysis, block: (Timeout.T, Analysis) => Unit): Option[Double] =
        System.gc()
        val to = timeout()
        val time = Timer.timeOnly(block(to, analysis))
        if to.reached then None
        else Some(time.toDouble / 1000000) // Return time in ms.

    def warmUp(msg: String, block: Timeout.T => Unit): Unit =
        print(s"Warmup: $msg ")
        val timeOut = timeout()
        for w <- 1 to maxWarmupRuns do
            print(s"$w ")
            System.gc()
            block(timeOut)
            if timeOut.reached then
                println()
                return
        println()

    def runNTimes(
        msg: String,
        createAnalysis: () => Analysis,
        block: (Timeout.T, Analysis) => Unit
      ): Option[List[Double]] =
        print(s"Measuring: $msg ")
        var times: List[Double] = List()
        for i <- 1 to measuredRuns do
            print(s"$i ")
            val analysis = createAnalysis() // Don't measure analysis creation.
            runOneTime(analysis, block) match
                case Some(t) => times = t :: times
                case None =>
                  println(" timed out.")
                  return None
        println()
        Some(times)

    // A single program run with the analysis.
    def onBenchmark(file: String): Unit =
        println(s"\nTesting $file")
        val program = parse(file)

        var times: Map[String, List[Double]] = Map().withDefaultValue(List.empty)
        var timeOuts: Map[String, Boolean] = Map().withDefaultValue(false)

        // Initial analysis.

        warmUp("initial analysis", timeout => analysis(program, noOptimisations.disableAsserts()).analyzeWithTimeout(timeout))
        runNTimes("initial analysis",
                  () => analysis(program, noOptimisations.disableAsserts()),
                  (timeout, analysis) => analysis.analyzeWithTimeout(timeout)
        ) match
            case None =>
              results = results.add(file, columnName(timeS, initS), Timedout)
              return
            case Some(ts) =>
              val stats = Statistics.all(ts)
              results = results
                .add(file, columnName(timeS, initS), Value(scala.math.round(stats.mean)))
                .add(file, columnName(stdS, initS), Value(scala.math.round(stats.stddev)))

        // Full reanalysis.

        warmUp("reanalysis",
               timeout => {
                 val a = analysis(program, noOptimisations.disableAsserts())
                 a.version = New
                 a.analyzeWithTimeout(timeout)
               }
        )
        runNTimes(
          "reanalysis",
          () => {
            val a = analysis(program, noOptimisations.disableAsserts())
            a.version = New
            a
          },
          (timeout, analysis) => analysis.analyzeWithTimeout(timeout)
        ) match
            case None => results = results.add(file, columnName(timeS, reanS), Timedout)
            case Some(ts) =>
              val stats = Statistics.all(ts)
              results = results
                .add(file, columnName(timeS, reanS), Value(scala.math.round(stats.mean)))
                .add(file, columnName(stdS, reanS), Value(scala.math.round(stats.stddev)))

        // Incremental measurements.

        // Run the initial analysis.
        val initAnalysis = analysis(program, allOptimisations.disableAsserts()) // Allow all caches to be initialised (may increase memory footprint).
        initAnalysis.analyzeWithTimeout(timeout())
        if !initAnalysis.finished then return

        configurations.foreach { config =>
            warmUp(config.toString,
                   timeout => {
                     val a = initAnalysis.deepCopy()
                     a.configuration = config
                     a.updateAnalysis(timeout)
                   }
            )
            runNTimes(config.toString,
                      () => {
                        val a = initAnalysis.deepCopy()
                        a.configuration = config
                        a
                      },
                      (timeout, analysis) => analysis.updateAnalysis(timeout)
            ) match
                case None => results = results.add(file, columnName(timeS, config.toString), Timedout)
                case Some(ts) =>
                  val stats = Statistics.all(ts)
                  results = results
                    .add(file, columnName(timeS, config.toString), Value(scala.math.round(stats.mean)))
                    .add(file, columnName(stdS, config.toString), Value(scala.math.round(stats.stddev)))
        }
    end onBenchmark

    def createOutput(): String =
        val cols = (List(initS, reanS) ++ configurations.map(_.toString)).flatMap(c => List(columnName(timeS, c), columnName(stdS, c)))
        results.prettyString(columns = cols, rowName = "benchmark") ++ "\n\n" ++ results.toCSVString(columns = cols, rowName = "benchmark")

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePerformance extends IncrementalTime[SchemeExp]:
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))
    override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeModFPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
    val outputFile: String = s"performance/modf-type.txt"

object IncrementalSchemeModFCPPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
    val outputFile: String = s"performance/modf-CP.txt"

object IncrementalSchemeModConcPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
    val outputFile: String = s"performance/modconc-type.txt"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModConcCPPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
    val outputFile: String = s"performance/modconc-CP.txt"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModXPerformance:
    def main(args: Array[String]): Unit =
      IncrementalSchemeModFPerformance.main(args)
//IncrementalSchemeModFCPPerformance.main(args)
//IncrementalSchemeModConcPerformance.main(args)
//IncrementalSchemeModConcCPPerformance.main(args)
