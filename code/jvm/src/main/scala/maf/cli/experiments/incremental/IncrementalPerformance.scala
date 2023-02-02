package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.*
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain.modularLattice
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice
import maf.modular.scheme.*
import maf.modular.scheme.modf.SchemeModFComponent
import maf.util.ColouredFormatting.*
import maf.util.*
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
trait IncrementalTime[E <: Expression] extends IncrementalExperiment[E] with TableOutput[E, Result]:

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

    def runOneTime(analysis: Analysis, block: (Timeout.T, Analysis) => Unit, t: Option[Timeout.T] = None): Option[Double] =
        System.gc()
        val to = t match { // Use a new timeout if none is given. This way, one timeout can be used for the entire warm-up.
            case None => timeout()
            case Some(t) => t
        }
        val time = Timer.timeOnly(block(to, analysis))
        if to.reached then None
        else Some(time.toDouble / 1000000) // Return time in ms.

    def runNTimes(
        msg: String,
        createAnalysis: () => Analysis,
        block: (Timeout.T, Analysis) => Unit,
        timeOut: Option[Timeout.T]
      ): Option[(List[Double], List[Double])] =
        print(s"$msg ")
        var times: List[Double] = List()
        var timesIntra: List[Double] = List()
        for i <- 1 to measuredRuns do
            print(s"$i ")
            val analysis = createAnalysis() // Don't measure analysis creation.
            runOneTime(analysis, block, timeOut) match
                case Some(t) =>
                    times = t :: times
                    timesIntra = (analysis.intraComponentAnalysisTimeAcc.toDouble / 1000000) :: timesIntra
                case None =>
                    println(markWarning(" timed out."))
                    return None
        println()
        Some((times, timesIntra))

    var first = true
    lazy val cols = columns // (List(initS, reanS) ++ configurations.map(_.toString)).flatMap(c => List(columnName(timeS, c), columnName(stdS, c), columnName(timeIntraS, c), columnName(stdIntraS, c)))

    def addToResults(file: String, times: Option[(List[Double], List[Double])], name: String): Boolean = times match {
        case None =>
            results = results.add(file, columnName(timeS, name), Timedout)
            true
        case Some((ts, tsi)) =>
            val stats = Statistics.all(ts)
            val statsIntra = Statistics.all(tsi)
            results = results
                .add(file, columnName(timeS, name), Value(scala.math.round(stats.mean)))
                .add(file, columnName(stdS, name), Value(scala.math.round(stats.stddev)))
                .add(file, columnName(timeIntraS, name), Value(scala.math.round(statsIntra.mean)))
                .add(file, columnName(stdIntraS, name), Value(scala.math.round(statsIntra.stddev)))
            false
    }

    // A single program run with the analysis.
    def onBenchmark(file: String): Unit =
        try
            results = Table.empty.withDefaultValue(NotRun)
            if first then
                Writer.disableReporting(output)
                Writer.writeln(output, results.toCSVString(columns = cols, rowName = "benchmark"))
                first = false

            val program = parse(file)

            // Initial analysis.

            runNTimes("Warming up initial analysis",
                () => analysis(program, noOptimisations.disableAsserts()),
                (timeout, analysis) => analysis.analyzeWithTimeout(timeout), Some(timeout())) // For the warm-up, use always use the same timeout.
            val initTs = runNTimes("Measuring initial analysis",
                () => analysis(program, noOptimisations.disableAsserts()),
                (timeout, analysis) => analysis.analyzeWithTimeout(timeout), None)
            if addToResults(file, initTs, initS)
            then return() // There was a timeout, so return. We cannot do anything anyway here, as probably no incremental updates can be run. TODO maybe remove?

            // Full reanalysis.

            runNTimes("Warming up reanalysis",
                () => { val a = analysis(program, noOptimisations.disableAsserts()); a.version = New; a },
                (timeout, analysis) => analysis.analyzeWithTimeout(timeout), Some(timeout()))
             val reanTs = runNTimes("Measuring reanalysis",
                 () => { val a = analysis(program, noOptimisations.disableAsserts()); a.version = New; a },
                 (timeout, analysis) => analysis.analyzeWithTimeout(timeout), None)
             addToResults(file, reanTs, reanS)

            // Incremental measurements.

            // Run the initial analysis.
            val initAnalysis = analysis(program, ci_di_wi.disableAsserts()) // Allow all caches to be initialised (may increase memory footprint). TODO add cy here!
            initAnalysis.analyzeWithTimeout(timeout())
            if !initAnalysis.finished then return () // Put unit explicitly to stop the formatter from putting the next line here.

            initAnalysis.intraComponentAnalysisTimeAcc = 0 // Reset the timer.

            configurations.foreach { config =>
                runNTimes(s"Warming up ${config.toString}",
                    () => { val a = initAnalysis.deepCopy(); a.configuration = config.disableAsserts(); a },
                    (timeout, analysis) => analysis.updateAnalysis(timeout), Some(timeout()))
                val incTs = runNTimes(s"Measuring ${config.toString}",
                    () => { val a = initAnalysis.deepCopy(); a.configuration = config.disableAsserts(); a },
                    (timeout, analysis) => analysis.updateAnalysis(timeout), None)
                addToResults(file, incTs, config.toString)
            }
            val lst: List[String] = results.toCSVString(columns = cols).split("\n").nn.toList.map(_.nn)
            Writer.writeln(output, lst(1))
        catch
            case _ =>
                reportError(file)
                val lst: List[String] = results.toCSVString(columns = cols).split("\n").nn.toList.map(_.nn)
                Writer.writeln(output, lst(1))
    end onBenchmark

    def createOutput(): String = "" // Results are written during benchmarking.

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePerformance extends IncrementalTime[SchemeExp]:
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))

class IncrementalSchemeModFTypePerformance() extends IncrementalSchemePerformance:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
        with SplitPerformance[SchemeExp] {
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modf-type.csv"

class IncrementalSchemeModFCPPerformance() extends IncrementalSchemePerformance:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
        with SplitPerformance[SchemeExp] {
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modf-CP.csv"

class IncrementalSchemeModConcTypePerformance() extends IncrementalSchemePerformance:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
        with SplitPerformance[SchemeExp] {
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modconc-type.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

class IncrementalSchemeModConcCPPerformance() extends IncrementalSchemePerformance:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
        with SplitPerformance[SchemeExp] {
        override def intraAnalysis(cmp: Component) =
            new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra with IncrementalGlobalStoreIntraAnalysis with SplitPerformanceIntra
    }
    val outputFile: String = s"performance/modconc-CP.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModXPerformance:

    def runPerformanceExperiment(exp: IncrementalTime[_], bench: Set[String], args: IncArgs, out: String): Unit =
        exp.maxWarmupRuns = args.warmUp
        exp.measuredRuns = args.repetitions
        val res = exp.execute(bench, args)
        FileOps.copy(res, out)

    def main(args: IncArgs): Unit =
        val outDir: String = "benchOutput/"

        val (curatedSuite, generatedSuite) = args.count match {
            case Some(n) => (IncrementalSchemeBenchmarkPrograms.sequentialCurated.take(n), IncrementalSchemeBenchmarkPrograms.sequentialGenerated.take(n))
            case None => (IncrementalSchemeBenchmarkPrograms.sequentialCurated, IncrementalSchemeBenchmarkPrograms.sequentialGenerated)
        }

        if args.typeLattice then
            if args.curated then runPerformanceExperiment(new IncrementalSchemeModFTypePerformance(), curatedSuite, args, outDir + "type-curated-performance.csv")
            if args.generated then runPerformanceExperiment(new IncrementalSchemeModFTypePerformance(), generatedSuite, args, outDir + "type-generated-performance.csv")
            if args.files.nonEmpty then runPerformanceExperiment(new IncrementalSchemeModFTypePerformance(), args.files, args, outDir + "type-file-performance.csv")

        if args.cpLattice then
            if args.curated then runPerformanceExperiment(new IncrementalSchemeModFCPPerformance(), curatedSuite, args, outDir + "cp-curated-performance.csv")
            if args.generated then runPerformanceExperiment(new IncrementalSchemeModFCPPerformance(), generatedSuite, args, outDir + "cp-generated-performance.csv")
            if args.files.nonEmpty then runPerformanceExperiment(new IncrementalSchemeModFCPPerformance(), args.files, args, outDir + "cp-file-performance.csv")
    end main
end IncrementalSchemeModXPerformance
