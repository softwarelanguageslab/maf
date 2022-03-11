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
    var resultsNoOpt: Table[String] = Table.empty.withDefaultValue(" ")
    val error: String = errS

    override def reportError(file: String): Unit = columns.foreach { c =>
        results = results.add(file, c, error)
        resultsNoOpt = resultsNoOpt.add(file, c, error)
    }

    def runAnalysis(name: String, block: Timeout.T => Unit): Boolean =
        print(name)
        val timeOut = timeout()
        block(timeOut)
        timeOut.reached // We do not use the test `analysis.finished`, as even though the WL can be empty, an intra-component analysis may also have been aborted.

    def compareAnalyses(
        file: String,
        inc: Analysis,
        rean: Analysis,
        table: Table[String]
      ): Table[String] =
        val cName = inc.configuration.toString
        // Both analyses normally share the same lattice, allocation schemes,... which makes it unnecessary to convert values etc.
        val iStore = inc.store.filterNot(kv => inc.lattice.isBottom(kv._2)).withDefaultValue(inc.lattice.bottom)
        val rStore = rean.store.filterNot(kv => rean.lattice.isBottom(kv._2)).withDefaultValue(rean.lattice.bottom)

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
              //System.err.nn.println(s"$a: $incr < $rean") // Soundness error.
              //System.err.nn.flush()
              m += 1 // The incremental value is subsumed by the value of the full reanalysis => more precise.
            }
        })
        table
          .add(file, columnName(eqS, cName), Formatter.withPercent(e, t))
          .add(file, columnName(lpS, cName), Formatter.withPercent(l, t))
          .add(file, columnName(mpS, cName), Formatter.withPercent(m, t))

    def onBenchmark(file: String): Unit =
        compareToFullReanalysis(file)
        println()
        compareToNoOptimisations(file)

    def compareToFullReanalysis(file: String): Unit =
        print(s"Testing $file (full R) ")
        val program = parse(file)
        // Initial analysis: analyse + update.
        val a1 = analysis(program, ci_di_wi) // Allow tracking for all optimisations.

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
            if !runAnalysis(config.toString + " ", timeOut => copy.updateAnalysis(timeOut)) then results = compareAnalyses(file, copy, a2, results)
            else
                propertiesS.foreach(o => results = results.add(file, columnName(o, config.toString), infS))
                print(" timed out - ")
        }

    def compareToNoOptimisations(file: String): Unit =
        print(s"Testing $file (noOpt) ")
        val program = parse(file)
        // Initial analysis: analyse + update.
        val a1 = analysis(program, ci_di_wi) // Allow tracking for all optimisations.

        // Run the initial analysis which needs to finish.
        if runAnalysis("init ", timeOut => a1.analyzeWithTimeout(timeOut)) then
            print("timed out.")
            columns.foreach(c => resultsNoOpt = resultsNoOpt.add(file, c, infS))
            return

        // Update the analysis without optimisations. Needs to finish as well.
        val a2 = a1.deepCopy()
        a2.configuration = noOptimisations
        if runAnalysis("noOpt ", timeOut => a2.updateAnalysis(timeOut)) then
            print("timed out.")
            columns.foreach(c => resultsNoOpt = resultsNoOpt.add(file, c, infS))
            return

        configurations.foreach { config =>
            val copy = a1.deepCopy()
            copy.configuration = config
            if !runAnalysis(config.toString + " ", timeOut => copy.updateAnalysis(timeOut)) then
                resultsNoOpt = compareAnalyses(file, copy, a2, resultsNoOpt)
            else
                propertiesS.foreach(o => resultsNoOpt = resultsNoOpt.add(file, columnName(o, config.toString), infS))
                print(" timed out - ")
        }

    def interestingAddress[A <: Address](a: A): Boolean
    def createOutput(): String =
      s"Compared to full reanalysis:\n${results.toCSVString(columns = columns, rowName = "benchmark")}\n\nCompared to noOpt:\n${resultsNoOpt
        .toCSVString(columns = columns, rowName = "benchmark")}"

    override def execute(args: Array[String]): String =
      // Clear the state.
      results = Table.empty.withDefaultValue(" ")
      resultsNoOpt = Table.empty.withDefaultValue(" ")
      super.execute(args)

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp]:
    override def interestingAddress[A <: Address](a: A): Boolean = a match
        case PrmAddr(_) => false
        case _          => true
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))
    override def timeout(): Timeout.T = Timeout.start(Duration(30, MINUTES))
    val configurations: List[IncrementalConfiguration] = allConfigurations

object IncrementalSchemeModFTypePrecision extends IncrementalSchemePrecision:
    override def benchmarks(): Set[String] = IncrementalSchemeBenchmarkPrograms.sequential //Generated
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
    def splitOutput(output: String, fullName: String = "", nooptName: String = ""): (String, String) =
        val text: List[List[String]] = Reader.loadFile(output).split("\n\n").nn.toList.map(_.nn.split("\n").nn.toList.map(_.nn).tail)
        if !(text.length == 2) then throw new Exception("Unexpected format")

        var outFull = s"${output.split("\\.").nn.head}-FULL.csv"
        var outNoOpt = s"${output.split("\\.").nn.head}-NOOPT.csv"

        if fullName.nonEmpty && nooptName.nonEmpty then
            outFull = fullName
            outNoOpt = nooptName
        end if

        val full = text.head.mkString("\n")
        val noOpt = text(1).mkString("\n")
        val fullW = Writer.open(outFull)
        Writer.write(fullW, full)
        Writer.close(fullW)
        val noOptW = Writer.open(outNoOpt)
        Writer.write(noOptW, noOpt)
        Writer.close(noOptW)
        (outFull, outNoOpt)
    end splitOutput

    def main(args: IncArgs): Unit =
        val outDir: String = "benchOutput/"

        val (curatedSuite, generatedSuite) = args.count match {
          case Some(n) => (IncrementalSchemeBenchmarkPrograms.sequential.take(n).toArray, IncrementalSchemeBenchmarkPrograms.sequentialGenerated.take(n).toArray)
          case None => (IncrementalSchemeBenchmarkPrograms.sequential.toArray, IncrementalSchemeBenchmarkPrograms.sequentialGenerated.toArray)
        }

        if args.typeLattice then
            if args.curated then splitOutput(IncrementalSchemeModFTypePrecision.execute(curatedSuite), s"${outDir}type-curated-precision.csv", s"${outDir}type-curated-precision-noopt.csv")
            if args.generated then splitOutput(IncrementalSchemeModFTypePrecision.execute(generatedSuite), s"${outDir}type-generated-precision.csv", s"${outDir}type-generated-precision-noopt.csv")
        end if
        if args.cpLattice then
            if args.curated then splitOutput(IncrementalSchemeModFCPPrecision.execute(curatedSuite), s"${outDir}cp-curated-precision.csv", s"${outDir}cp-curated-precision-noopt.csv")
            if args.generated then splitOutput(IncrementalSchemeModFCPPrecision.execute(generatedSuite), s"${outDir}cp-generated-precision.csv", s"${outDir}cp-generated-precision-noopt.csv")
        end if
    end main
end IncrementalSchemeModXPrecision
