package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.{Expression, Identifier}
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain.modularLattice
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice
import maf.modular.scheme.{PtrAddr, VarAddr}
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.{FileOps, Reader, Writer}
import maf.util.benchmarks.*

import java.nio.file.StandardCopyOption
import scala.concurrent.duration.*

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

trait SplitPerformance[E <: Expression] extends IncrementalModAnalysis[E] with IncrementalGlobalStore[E]:
    /** Accumulated intra-component analysis time. */
    var intraComponentAnalysisTimeAcc: Long = 0

    trait SplitPerformanceIntra extends IncrementalIntraAnalysis with IncrementalGlobalStoreIntraAnalysis:
        abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
          intraComponentAnalysisTimeAcc = intraComponentAnalysisTimeAcc + Timer.timeOnly(super.analyzeWithTimeout(timeout))
    end SplitPerformanceIntra

    override def configString(): String = super.configString() + s"\n  splitting performance measurements"
end SplitPerformance

// TODO: current error handling is incorrect.
trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] with TableOutput[Result]:

    type Analysis = IncrementalModAnalysis[E] with IncrementalGlobalStore[E] with SplitPerformance[E]

    // The maximal number of warm-up runs.
    var maxWarmupRuns = 3 //5
    // The number of actually measured runs.
    var measuredRuns = 15 //30

    val timeS: String = "ms" // Mean of measured times.
    val stdS: String = "SD" // Standard deviation of mean.
    val timeIntraS: String = "intraMS" // Mean of the measured intra-component analysis times.
    val stdIntraS: String = "intraSD" // Standard deviation of the mean.
    val propertiesS: List[String] = List(timeS, stdS, timeIntraS, stdIntraS)
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
                return ()
        println()

    def runNTimes(
        msg: String,
        createAnalysis: () => Analysis,
        block: (Timeout.T, Analysis) => Unit
      ): Option[(List[Double], List[Double])] =
        print(s"Measuring: $msg ")
        var times: List[Double] = List()
        var timesIntra: List[Double] = List()
        for i <- 1 to measuredRuns do
            print(s"$i ")
            val analysis = createAnalysis() // Don't measure analysis creation.
            runOneTime(analysis, block) match
                case Some(t) =>
                  times = t :: times
                  timesIntra = (analysis.intraComponentAnalysisTimeAcc.toDouble / 1000000) :: timesIntra
                case None =>
                  println(" timed out.")
                  return None
        println()
        Some((times, timesIntra))

    var first = true
    lazy val cols = columns // (List(initS, reanS) ++ configurations.map(_.toString)).flatMap(c => List(columnName(timeS, c), columnName(stdS, c), columnName(timeIntraS, c), columnName(stdIntraS, c)))

    // A single program run with the analysis.
    def onBenchmark(file: String): Unit =
      try
          results = Table.empty.withDefaultValue(NotRun)
          if first then
              Writer.disableReporting()
              Writer.writeln(results.toCSVString(columns = cols, rowName = "benchmark"))
              first = false

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
              case Some((ts, tsi)) =>
                val stats = Statistics.all(ts)
                val statsIntra = Statistics.all(tsi)
                results = results
                  .add(file, columnName(timeS, initS), Value(scala.math.round(stats.mean)))
                  .add(file, columnName(stdS, initS), Value(scala.math.round(stats.stddev)))
                  .add(file, columnName(timeIntraS, initS), Value(scala.math.round(statsIntra.mean)))
                  .add(file, columnName(stdIntraS, initS), Value(scala.math.round(statsIntra.stddev)))

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
              case Some((ts, tsi)) =>
                val stats = Statistics.all(ts)
                val statsIntra = Statistics.all(tsi)
                results = results
                  .add(file, columnName(timeS, reanS), Value(scala.math.round(stats.mean)))
                  .add(file, columnName(stdS, reanS), Value(scala.math.round(stats.stddev)))
                  .add(file, columnName(timeIntraS, reanS), Value(scala.math.round(statsIntra.mean)))
                  .add(file, columnName(stdIntraS, reanS), Value(scala.math.round(statsIntra.stddev)))

          // Incremental measurements.

          // Run the initial analysis.
          val initAnalysis = analysis(program, ci_di_wi.disableAsserts()) // Allow all caches to be initialised (may increase memory footprint).
          initAnalysis.analyzeWithTimeout(timeout())
          if !initAnalysis.finished then return () // Put unit explicitly to stop the formatter from putting the next line here.

          initAnalysis.intraComponentAnalysisTimeAcc = 0 // Reset the timer.

          configurations.foreach { config =>
              warmUp(config.toString,
                     timeout => {
                       val a = initAnalysis.deepCopy()
                       a.configuration = config.disableAsserts()
                       a.updateAnalysis(timeout)
                     }
              )
              runNTimes(config.toString,
                        () => {
                          val a = initAnalysis.deepCopy()
                          a.configuration = config.disableAsserts()
                          a
                        },
                        (timeout, analysis) => analysis.updateAnalysis(timeout)
              ) match
                  case None => results = results.add(file, columnName(timeS, config.toString), Timedout)
                  case Some((ts, tsi)) =>
                    val stats = Statistics.all(ts)
                    val statsIntra = Statistics.all(tsi)
                    results = results
                      .add(file, columnName(timeS, config.toString), Value(scala.math.round(stats.mean)))
                      .add(file, columnName(stdS, config.toString), Value(scala.math.round(stats.stddev)))
                      .add(file, columnName(timeIntraS, config.toString), Value(scala.math.round(statsIntra.mean)))
                      .add(file, columnName(stdIntraS, config.toString), Value(scala.math.round(statsIntra.stddev)))
          }
          val lst: List[String] = results.toCSVString(columns = cols).split("\n").nn.toList.map(_.nn)
          Writer.writeln(lst(1))
      catch
          case _ =>
            reportError(file)
            val lst: List[String] = results.toCSVString(columns = cols).split("\n").nn.toList.map(_.nn)
            Writer.writeln(lst(1))
    end onBenchmark

    def createOutput(): String = "" // Results are written during benchmarking.

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePerformance extends IncrementalTime[SchemeExp]:
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))
    override def timeout(): Timeout.T = Timeout.start(Duration(30, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeModFTypePerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential //Generated
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
      with SplitPerformance[SchemeExp] {
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modf-type.csv"

object IncrementalSchemeModFCPPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
      with SplitPerformance[SchemeExp] {
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modf-CP.csv"

object IncrementalSchemeModConcTypePerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
      with SplitPerformance[SchemeExp] {
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modconc-type.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModConcCPPerformance extends IncrementalSchemePerformance:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
      with SplitPerformance[SchemeExp] {
      override def intraAnalysis(cmp: Component) =
        new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modconc-CP.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModXPerformance:
    def main(args: IncArgs): Unit =
        val outDir: String = "benchOutput/"

        val (curatedSuite, generatedSuite) = args.count match {
          case Some(n) => (IncrementalSchemeBenchmarkPrograms.sequential.take(n).toArray, IncrementalSchemeBenchmarkPrograms.sequentialGenerated.take(n).toArray)
          case None => (IncrementalSchemeBenchmarkPrograms.sequential.toArray, IncrementalSchemeBenchmarkPrograms.sequentialGenerated.toArray)
        }

        if args.typeLattice then
            IncrementalSchemeModFTypePerformance.maxWarmupRuns = args.warmUp
            IncrementalSchemeModFTypePerformance.measuredRuns = args.repetitions

            if args.curated then
                val curatedType = IncrementalSchemeModFTypePerformance.execute(curatedSuite)
                FileOps.copy(curatedType, outDir + "type-curated-performance.csv")
            end if
            if args.generated then
                IncrementalSchemeModFTypePerformance.first = true
                val generatedType = IncrementalSchemeModFTypePerformance.execute(generatedSuite)
                FileOps.copy(generatedType, outDir + "type-generated-performance.csv")
            end if
        end if

        if args.cpLattice then
            IncrementalSchemeModFCPPerformance.maxWarmupRuns = args.warmUp
            IncrementalSchemeModFCPPerformance.measuredRuns = args.repetitions

            if args.curated then
                val curatedCP = IncrementalSchemeModFCPPerformance.execute(curatedSuite)
                FileOps.copy(curatedCP, outDir + "cp-curated-performance.csv")
            end if
            if args.generated then
                IncrementalSchemeModFCPPerformance.first = true
                val generatedCP = IncrementalSchemeModFCPPerformance.execute(generatedSuite)
                FileOps.copy(generatedCP, outDir + "cp-generated-performance.csv")
            end if
        end if
    end main
end IncrementalSchemeModXPerformance
