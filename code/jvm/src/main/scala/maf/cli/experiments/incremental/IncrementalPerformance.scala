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

case class Finished(mean: Long, stddev: Long) extends Result:
    override def toString: String = s"$mean±$stddev"

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

    val timeS: String = "ms"
    val propertiesS: List[String] = List(timeS)
    var results: Table[Result] = Table.empty.withDefaultValue(NotRun)
    val error: Result = Errored

    def runOneTime(analysis: Analysis, block: (Timeout.T, Analysis) => Unit): Option[Double] =
        System.gc()
        val to = timeout()
        val time = Timer.timeOnly(block(to, analysis))
        if to.reached then None
        else Some(time.toDouble / 1000000) // Return time in ms.

    def warmUp(
        msg: String,
        maxRepetitions: Int,
        timeOut: Timeout.T,
        block: Timeout.T => Unit
      ): Unit =
        print(s"Warmup: $msg ")
        for w <- 1 to maxRepetitions do
            print(s"$w ")
            System.gc()
            block(timeOut)
            if timeOut.reached then
                println()
                return
        println()

    def runNTimes(
        msg: String,
        repetitions: Int,
        createAnalysis: () => Analysis,
        block: (Timeout.T, Analysis) => Unit
      ): Option[List[Double]] =
        print(s"Measuring: $msg ")
        var times: List[Double] = List()
        for i <- 1 to repetitions do
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
    // TODO: Do not measure analysis initialisation or creation?
    def onBenchmark(file: String): Unit =
        println(s"\nTesting $file")
        val program = parse(file)

        var times: Map[String, List[Double]] = Map().withDefaultValue(List.empty)
        var timeOuts: Map[String, Boolean] = Map().withDefaultValue(false)

        // Initial analysis.

        warmUp("initial analysis", maxWarmupRuns, timeout(), timeout => analysis(program, noOptimisations.disableAsserts()).analyzeWithTimeout(timeout))
        runNTimes("initial analysis",
                  measuredRuns,
                  () => analysis(program, noOptimisations.disableAsserts()),
                  (timeout, analysis) => analysis.analyzeWithTimeout(timeout)
        ) match
            case None =>
              results = results
                .add(file, columnName(timeS, initS), Timedout)
                .add(file, columnName(timeS, reanS), NotRun)
              configurations.map(_.toString).foreach(name => results = results.add(file, columnName(timeS, name), NotRun))
              return
            case Some(ts) => times = times + (initS -> ts)

        // Full reanalysis.

        warmUp("reanalysis",
               maxWarmupRuns,
               timeout(),
               timeout => {
                 val a = analysis(program, noOptimisations.disableAsserts())
                 a.version = New
                 a.analyzeWithTimeout(timeout)
               }
        )
        runNTimes(
          "reanalysis",
          measuredRuns,
          () => {
            val a = analysis(program, noOptimisations.disableAsserts())
            a.version = New
            a
          },
          (timeout, analysis) => analysis.analyzeWithTimeout(timeout)
        ) match
            case None     => timeOuts = timeOuts + (reanS -> true)
            case Some(ts) => times = times + (reanS -> ts)

        // Incremental measurements.

        // Run the initial analysis.
        val initAnalysis = analysis(program, allOptimisations.disableAsserts()) // Allow tracking.
        initAnalysis.analyzeWithTimeout(timeout())
        if !initAnalysis.finished then
            configurations.map(_.toString).foreach(name => results = results.add(file, columnName(timeS, name), NotRun))
            return

        configurations.foreach { config =>
            warmUp(config.toString,
                   maxWarmupRuns,
                   timeout(),
                   timeout => {
                     val a = initAnalysis.deepCopy()
                     a.configuration = config
                     a.updateAnalysis(timeout)
                   }
            )
            runNTimes(
              config.toString,
              measuredRuns,
              () => {
                val a = initAnalysis.deepCopy()
                a.configuration = config
                a
              },
              (timeout, analysis) => analysis.updateAnalysis(timeout)
            ) match
                case None     => timeOuts = timeOuts + (config.toString -> true)
                case Some(ts) => times = times + (config.toString -> ts)
        }

        val statistics: Map[String, Statistics.Stats] = times.map({ case (k, v) => (k, Statistics.all(v)) })

        results = results
          .add(file, columnName(timeS, initS), Finished(scala.math.round(statistics(initS).mean), scala.math.round(statistics(initS).stddev)))
          .add(
            file,
            columnName(timeS, reanS),
            if timeOuts(reanS) then Timedout else Finished(scala.math.round(statistics(reanS).mean), scala.math.round(statistics(reanS).stddev))
          )
        configurations.foreach { config =>
            val name = config.toString
            results = results.add(
              file,
              columnName(timeS, name),
              if timeOuts(name) then Timedout else Finished(scala.math.round(statistics(name).mean), scala.math.round(statistics(name).stddev))
            )

        }

    def createOutput(): String =
      results.prettyString(columns = (List(initS, reanS) ++ configurations.map(_.toString)).map(columnName(timeS, _))) ++ "\n\n" ++ results
        .toCSVString(columns = (List(initS, reanS) ++ configurations.map(_.toString)).map(columnName(timeS, _)))

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

object IncrementalSchemeModConcCPPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
    val outputFile: String = s"performance/modconc-CP.txt"

object IncrementalSchemeModXPerformance:
    def main(args: Array[String]): Unit =
        IncrementalSchemeModFPerformance.main(args)
        IncrementalSchemeModFCPPerformance.main(args)
        IncrementalSchemeModConcPerformance.main(args)
        IncrementalSchemeModConcCPPerformance.main(args)
