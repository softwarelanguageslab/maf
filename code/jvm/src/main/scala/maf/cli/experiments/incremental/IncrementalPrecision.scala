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
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] with TableOutput[String]:

    type Analysis = IncrementalModAnalysis[E] with IncrementalGlobalStore[E]

    final val eqS: String = "Equal" // Precision of incremental update equals the one of a full reanalysis.
    final val mpS: String = "More precise" // Precision of incremental update is better than the one of a full reanalysis.
    final val lpS: String = "Less precise" // Precision of incremental update is lower than the one of a full reanalysis.

    final val propertiesS: List[String] = List(eqS, lpS, mpS)
    override lazy val analysesS: List[String] = configurations.map(_.toString)

    var results: Table[String] = Table.empty.withDefaultValue(" ")
    val error: String = errS

    def runAnalysis(name: String, block: Timeout.T => Unit): Boolean =
        print(name)
        val timeOut = timeout()
        block(timeOut)
        timeOut.reached // We do not use the test `analysis.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.

    def compareAnalyses(
        file: String,
        inc: Analysis,
        rean: Analysis
      ): Unit =
        val cName = inc.configuration.toString
        // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
        val iStore = inc.store.withDefaultValue(inc.lattice.bottom)
        val rStore = rean.store.withDefaultValue(rean.lattice.bottom)

        val allAddr = iStore.keySet.filter(interestingAddress) ++ rStore.keySet.filter(interestingAddress)
        var e: Long = 0L
        var l: Long = 0L
        var m: Long = 0L
        val t: Long = allAddr.size.toLong
        allAddr.foreach({ a =>
            val incr = iStore(a)
            val rean = rStore(a)
            if incr == rean then e += 1 // Both results are the same => equally precise.
            else if inc.lattice.subsumes(incr, rean.asInstanceOf[inc.Value]) then l += 1 // The incremental value subsumes the value of the full reanalysis => less precise.
            else {
              System.err.nn.println(s"$a: $incr < $rean") // Soundness error.
              System.err.nn.flush()
              m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
            }
        })
        results = results
          .add(file, columnName(eqS, cName), Formatter.withPercent(e, t))
          .add(file, columnName(lpS, cName), Formatter.withPercent(l, t))
          .add(file, columnName(mpS, cName), Formatter.withPercent(m, t))

    def onBenchmark(file: String): Unit =
        print(s"Testing $file ")
        val program = parse(file)
        // Initial analysis: analyse + update.
        val a1 = analysis(program, allOptimisations) // Allow tracking for all optimisations.

        // Base case: analysis of new program version.
        val a2 = analysis(program, noOptimisations) // The configuration does not matter here.
        a2.version = New

        // Run the initial analysis and full reanalysis. They both need to finish.
        if runAnalysis("init ", timeOut => a1.analyzeWithTimeout(timeOut)) || runAnalysis("rean ", timeOut => a2.analyzeWithTimeout(timeOut)) then
            print("timed out.")
            columns.foreach(c => results = results.add(file, c, infS))
            return

        configurations.foreach { config =>
            val copy = a1.deepCopy()
            copy.configuration = config

            if !runAnalysis(config.toString + " ", timeOut => copy.updateAnalysis(timeOut)) then compareAnalyses(file, copy, a2)
            else
                propertiesS.foreach(o => results = results.add(file, columnName(o, config.toString), infS))
                print(" timed out - ")
        }

    // Note, we could also compare to the initial analysis. This would give us an idea on how many addresses were refined (column "More precise").

    def interestingAddress[A <: Address](a: A): Boolean
    def createOutput(): String = results.prettyString(columns = columns) ++ "\n\n" ++ results.toCSVString(columns = columns)

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp]:
    override def interestingAddress[A <: Address](a: A): Boolean = a match
        case PrmAddr(_) => false
        case _          => true
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))
    override def timeout(): Timeout.T = Timeout.start(Duration(2, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeModFTypePrecision extends IncrementalSchemePrecision:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
    val outputFile: String = "precision/modf-type.txt"

object IncrementalSchemeModFCPPrecision extends IncrementalSchemePrecision:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
    val outputFile: String = "precision/modf-CP.txt"

object IncrementalSchemeModConcTypePrecision extends IncrementalSchemePrecision:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
    val outputFile: String = "precision/modconc-type.txt"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModConcCPPrecision extends IncrementalSchemePrecision:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.threads
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
    val outputFile: String = "precision/modconc-CP.txt"
    override val configurations: List[IncrementalConfiguration] = allConfigurations.filterNot(_.cyclicValueInvalidation)

object IncrementalSchemeModXPrecision:
    def main(args: Array[String]): Unit =
        IncrementalSchemeModFTypePrecision.main(args)
        IncrementalSchemeModFCPPrecision.main(args)
        IncrementalSchemeModConcTypePrecision.main(args)
        IncrementalSchemeModConcCPPrecision.main(args)
