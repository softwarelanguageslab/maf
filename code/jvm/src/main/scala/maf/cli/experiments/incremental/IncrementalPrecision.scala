package maf.cli.experiments.incremental

import maf.bench.scheme.IncrementalSchemeBenchmarkPrograms
import maf.core.*
import maf.language.CScheme.CSchemeParser
import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.incremental.IncrementalConfiguration.*
import maf.modular.incremental.*
import maf.modular.incremental.scheme.IncrementalSchemeAnalysisInstantiations.*
import maf.modular.scheme.*
import maf.util.*
import maf.util.ColouredFormatting.*
import maf.util.benchmarks.*

import scala.concurrent.duration.*

trait IncrementalPrecision[E <: Expression] extends IncrementalExperiment[E] with TableOutput[E, String]:

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
        table: Table[String],
        printResult: Boolean // Indicates whether the result overview (OK/NOT OK) is printed to the terminal.
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
        if printResult
        then print((l, m) match {
            case (0, 0) =>
                markOK("PRECISE ")
            case (_, 0) =>
                markError("IMPRECISE ")
            case (0, _) =>
                markBad("UNSOUND ")
            case _ =>
                markError("IMPRECISE ")
                markBad("UNSOUND ")
        })
        table
            .add(file, columnName(eqS, cName), Formatter.withPercent(e, t))
            .add(file, columnName(lpS, cName), Formatter.withPercent(l, t))
            .add(file, columnName(mpS, cName), Formatter.withPercent(m, t))

    def onBenchmark(file: String): Unit =
        print(markStep(s"Testing precision: "))
        val program = parse(file)
        // Initial analysis: analyse + update.
        val init = analysis(program, allOptimisations) // Allow tracking for all optimisations.

        // Run the initial analsyis and make sure it finishes.
        if runAnalysis(s"init ", timeOut => init.analyzeWithTimeout(timeOut)) then
            print(markWarning("TIMEOUT"))
            columns.foreach { c =>
                resultsNoOpt = resultsNoOpt.add(file, c, infS)
                results = results.add(file, c, infS)
            }
            return

        // Full reanalysis of the program.
        val rean = analysis(program, noOptimisations) // The configuration does not matter here.
        rean.version = New
        var reanTO = false

        if runAnalysis("rean ", timeOut => rean.analyzeWithTimeout(timeOut)) then
            print(markWarning("TIMEOUT "))
            columns.foreach(c => results = results.add(file, c, infS))
            reanTO = true

        // Incremental update without optimisations.
        val noOpt = init.deepCopy()
        noOpt.configuration = noOptimisations
        var noOptTO = false

        if runAnalysis("noOpt ", timeOut => noOpt.updateAnalysis(timeOut)) then
            print(markWarning("TIMEOUT "))
            columns.foreach(c => resultsNoOpt = resultsNoOpt.add(file, c, infS))
            noOptTO = true

        // Abort if nothing to compare to.
        if reanTO && noOptTO then { println(); return }

        // Run the incremental update using each configuration and compare.
        configurations.foreach { config =>
            val copy = init.deepCopy()
            copy.configuration = config
            if !runAnalysis(config.toString + " ", timeOut => copy.updateAnalysis(timeOut)) then
                results = compareAnalyses(file, copy, rean, results, true) // Print the status for the comparison to a full reanalysis to the terminal.
                resultsNoOpt = compareAnalyses(file, init, noOpt, resultsNoOpt, false)
            else
                print(markWarning("TIMEOUT "))
                propertiesS.foreach { o =>
                    results = results.add(file, columnName(o, config.toString), infS)
                    resultsNoOpt = resultsNoOpt.add(file, columnName(o, config.toString), infS)
                }
        }
    end onBenchmark

    def interestingAddress[A <: Address](a: A): Boolean
    def createOutput(): String =
        s"Compared to full reanalysis:\n${results.toCSVString(columns = columns, rowName = "benchmark")}\n\nCompared to noOpt:\n${resultsNoOpt
            .toCSVString(columns = columns, rowName = "benchmark")}"

/* ************************** */
/* ***** Instantiations ***** */
/* ************************** */

trait IncrementalSchemePrecision extends IncrementalPrecision[SchemeExp]:
    override def interestingAddress[A <: Address](a: A): Boolean = a match
        case PrmAddr(_) => false
        case _          => true
    override def parse(string: String): SchemeExp = CSchemeParser.parseProgram(Reader.loadFile(string))

class IncrementalSchemeModFTypePrecision() extends IncrementalSchemePrecision:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisTypeLattice(e, config)
    val outputFile: String = "precision/modf-type.txt"

class IncrementalSchemeModFCPPrecision() extends IncrementalSchemePrecision:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalSchemeModFAnalysisCPLattice(e, config)
    val outputFile: String = "precision/modf-CP.txt"

class IncrementalSchemeModConcTypePrecision() extends IncrementalSchemePrecision:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisTypeLattice(e, config)
    val outputFile: String = "precision/modconc-type.txt"

class IncrementalSchemeModConcCPPrecision() extends IncrementalSchemePrecision:
    override def analysis(e: SchemeExp, config: IncrementalConfiguration): Analysis = new IncrementalModConcAnalysisCPLattice(e, config)
    val outputFile: String = "precision/modconc-CP.txt"

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
            case Some(n) =>
                (IncrementalSchemeBenchmarkPrograms.sequentialCurated.take(n), IncrementalSchemeBenchmarkPrograms.sequentialGenerated.take(n))
            case None => (IncrementalSchemeBenchmarkPrograms.sequentialCurated, IncrementalSchemeBenchmarkPrograms.sequentialGenerated)
        }

        if args.typeLattice then
            if args.curated then
                splitOutput(
                  (new IncrementalSchemeModFTypePrecision).execute(curatedSuite, args),
                  s"${outDir}type-curated-precision-vs-full.csv",
                  s"${outDir}type-curated-precision-vs-noopt.csv"
                )
            if args.generated then
                splitOutput(
                  (new IncrementalSchemeModFTypePrecision).execute(generatedSuite, args),
                  s"${outDir}type-generated-precision-vs-full.csv",
                  s"${outDir}type-generated-precision-vs-noopt.csv"
                )
            if args.files.nonEmpty then
                splitOutput(
                  (new IncrementalSchemeModFTypePrecision).execute(args.files, args),
                  s"${outDir}type-file-precision-vs-full.csv",
                  s"${outDir}type-file-precision-vs-noopt.csv"
                )
        end if
        if args.cpLattice then
            if args.curated then
                splitOutput(
                  (new IncrementalSchemeModFCPPrecision).execute(curatedSuite, args),
                  s"${outDir}cp-curated-precision-vs-full.csv",
                  s"${outDir}cp-curated-precision-vs-noopt.csv"
                )
            if args.generated then
                splitOutput(
                  (new IncrementalSchemeModFCPPrecision).execute(generatedSuite, args),
                  s"${outDir}cp-generated-precision-vs-full.csv",
                  s"${outDir}cp-generated-precision-vs-noopt.csv"
                )
            if args.files.nonEmpty then
                splitOutput(
                  (new IncrementalSchemeModFCPPrecision).execute(args.files, args),
                  s"${outDir}cp-file-precision-vs-full.csv",
                  s"${outDir}cp-file-precision-vs-noopt.csv"
                )
        end if
    end main
end IncrementalSchemeModXPrecision
