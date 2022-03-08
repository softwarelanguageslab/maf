package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core._
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.IncrementalConfiguration._
import maf.modular.incremental._
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations._
import maf.modular.scheme._
import maf.util.datastructures.ListOps.Crossable
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalProperties[E <: Expression] extends IncrementalExperiment[E] with TableOutput[String]:

    type Analysis = IncrementalModAnalysis[E] with IncrementalGlobalStore[E] with CountIntraAnalyses[E]

    final val co: String = "#Components" // Number of components
    final val an: String = "#Analyses" // Number of intra-component analyses
    final val ad: String = "|Store|" // Store size
    final val dp: String = "#Dependencies" // Number of dependencies
    final val ic: String = "Dir. imp. Cmp." // Number of directly impacted components.

    final val propertiesS = List(ad, co, dp, an) // Properties

    override lazy val columns: List[String] = propertiesS.cartesian(analysesS).map(e => columnName(e._1, e._2)).toList ::: List(ic)

    var results: Table[String] = Table.empty.withDefaultValue(" ")
    val error: String = errS

    def runAnalysis(
        file: String,
        analysis: Analysis,
        block: Timeout.T => Unit,
        name: String = ""
      ): Boolean =
        val cName = if name == "" then analysis.configuration.toString else name
        print(s"$cName ")
        val timeOut = timeout()
        block(timeOut)
        if timeOut.reached then // We do not use the test `analysis.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.
            print("timed out.")
            propertiesS.foreach(p => results = results.add(file, columnName(p, cName), infS))
            return false
        results = results
          .add(file, columnName(co, cName), s"${analysis.visited.size}")
          .add(file, columnName(an, cName), s"${analysis.intraAnalysisCount}")
          .add(file, columnName(ad, cName), s"${analysis.store.size}")
          .add(file, columnName(dp, cName), s"${analysis.deps.values.map(_.size).sum}")
        true

    def onBenchmark(file: String): Unit =
        print(s"Testing $file ")
        val program = parse(file)

        // Initial analysis: analyse + update.
        val a1 = analysis(program, ci_di_wi) // Allow tracking for all configurations!

        // Base case: analysis of new program version.
        val a2 = analysis(program, noOptimisations) // The configuration does not matter here.
        a2.version = New

        // Run the initial analysis.
        if !runAnalysis(file, a1, timeOut => a1.analyzeWithTimeout(timeOut), initS) then return

        a1.resetIntraAnalysisCount()

        // Count the number of immediately affected components.
        val immediatelyAffected = a1.findUpdatedExpressions(program).flatMap(a1.mapping)
        results = results.add(file, ic, s"${immediatelyAffected.size}")

        // Run the incremental updates for the different configurations.
        configurations.foreach { config =>
            val copy = a1.deepCopy() // a1 contains the analysis state after the initial analysis.
            copy.configuration = config // Set the right configuration for the incremental update.
            runAnalysis(file, copy, timeout => copy.updateAnalysis(timeout))
        }

        // Run a full reanalysis
        runAnalysis(file, a2, timeOut => a2.analyzeWithTimeout(timeOut), reanS)

    def interestingAddress[A <: Address](a: A): Boolean

    def createOutput(): String = //results.prettyString() ++ "\n\n" ++
      results.toCSVString(rowName = "benchmark")

/** Counts the number of intra-component analyses run by the analysis. */
trait CountIntraAnalyses[Expr <: Expression] extends IncrementalModAnalysis[Expr]:
    var intraAnalysisCount: Int = 0

    // Method of the sequential worklist algorithm.
    override def step(timeout: Timeout.T): Unit =
        intraAnalysisCount = intraAnalysisCount + 1
        super.step(timeout)

    def resetIntraAnalysisCount(): Unit = intraAnalysisCount = 0

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemeProperties extends IncrementalProperties[SchemeExp]:
    override def interestingAddress[A <: Address](a: A): Boolean = a match
        case PrmAddr(_) => false
        case _          => true
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))
    override def timeout(): Timeout.T = Timeout.start(Duration(10, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeModFTypeProperties extends IncrementalSchemeProperties:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequentialGenerated
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
      with CountIntraAnalyses[SchemeExp]
    val outputFile: String = s"properties/modf-type.csv"

object IncrementalSchemeModFCPProperties extends IncrementalSchemeProperties:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
      with CountIntraAnalyses[SchemeExp]
    val outputFile: String = s"properties/modf-CP.csv"

object IncrementalSchemeModConcTypeProperties extends IncrementalSchemeProperties:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
      with CountIntraAnalyses[SchemeExp]
    val outputFile: String = s"properties/modconc-type.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModConcCPProperties extends IncrementalSchemeProperties:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
      with CountIntraAnalyses[SchemeExp]
    val outputFile: String = s"properties/modconc-CP.csv"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModXProperties:
    def main(args: IncArgs): Unit =
        if args.typeLattice then
            if args.curated then IncrementalSchemeModFTypeProperties.execute(IncrementalSchemeBenchmarkPrograms.sequential.toArray)
            if args.generated then IncrementalSchemeModFTypeProperties.execute(IncrementalSchemeBenchmarkPrograms.sequentialGenerated.toArray)
        end if

        if args.cpLattice then
            if args.curated then IncrementalSchemeModFCPProperties.execute(IncrementalSchemeBenchmarkPrograms.sequential.toArray)
            if args.generated then IncrementalSchemeModFCPProperties.execute(IncrementalSchemeBenchmarkPrograms.sequentialGenerated.toArray)
        end if
    end main
end IncrementalSchemeModXProperties
