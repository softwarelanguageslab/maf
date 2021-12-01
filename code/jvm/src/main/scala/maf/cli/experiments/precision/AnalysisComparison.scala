package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

abstract class AnalysisComparison[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the base analysis (= lowest precision) to compare to
    // - the other analyses to compare to the base analysis
    def baseAnalysis(prg: SchemeExp): Analysis
    def otherAnalyses(): List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 5min.)
    def analysisTimeout(): Timeout.T = Timeout.start(Duration(45, MINUTES)) //timeout for (non-base) analyses
    def concreteTimeout(): Timeout.T = Timeout.none //timeout for concrete interpreter

    def concreteRuns() = 5

    // keep the results of the benchmarks in a table
    var results: Table[Option[Int]] = Table.empty[Option[Int]]

    /**
     * For a given benchmark, compare each analysis with the base analysis That is, count for each analysis how many values were refined w.r.t. the
     * base analysis All results are saved in the `result` table of this object
     *
     * @param path
     *   the name of / path to the benchmark program
     * @param program
     *   the Scheme expression of the benchmark program
     */
    protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit =
        // run the base analysis first
        val baseResult = runAnalysis(baseAnalysis, "base analysis", program, path) match
            case Terminated(res) => res
            case _               => throw new Exception("This should not happen for the base analysis!")
        // run the other analyses on the benchmark
        otherAnalyses().foreach { case (analysis, name) =>
          val otherResult = runAnalysis(analysis, name, program, path, analysisTimeout())
          val refined = otherResult match
              case Terminated(store) => Some(compareOrdered(baseResult, store).size)
              case _                 => None
          results = results.add(path, name, refined)
        }
        // run a concrete interpreter on the benchmarks
        val concreteResult = runInterpreter(program, path, concreteTimeout(), concreteRuns())
        val refined = concreteResult.map(store => compareOrdered(baseResult, store).size)
        results = results.add(path, "concrete", refined)

object AnalysisComparison1
    extends AnalysisComparison[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:
    val k = 0
    val l = 1000
    def baseAnalysis(prg: SchemeExp): Analysis =
      SchemeAnalyses.kCFAAnalysis(prg, k)
    def otherAnalyses() =
      // run some regular k-cfa analyses
      List(
        //(SchemeAnalyses.modflocalAnalysis(_, 0), "0-CFA DSS"),
        (SchemeAnalyses.modflocalAnalysisAdaptiveA(_, k, l), s"$k-CFA DSS w/ ASW (l = $l)")
      )

    def main(args: Array[String]) = runBenchmarks(
      Set(
        "test/R5RS/various/mceval.scm"
      )
    )

    def check(path: Benchmark) =
        val txt = Reader.loadFile(path)
        val prg = SchemeParser.parseProgram(txt)
        val con = runInterpreter(prg, path).get
        val Terminated(abs) = runAnalysis(SchemeAnalyses.fullArgContextSensitiveAnalysis(_), "analysis", prg, path)
        val allKeys = con.keys ++ abs.keys
        allKeys.foreach { k =>
            val absVal = abs.getOrElse(k, "⊥")
            val concVal = con.getOrElse(k, "⊥")
            if absVal != concVal then println(s"$k -> $absVal ; $concVal ")
        }

    def runBenchmarks(benchmarks: Set[Benchmark]) =
        benchmarks.foreach(runBenchmark)
        println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
        Writer.setDefaultWriter(Writer.open("benchOutput/precision/adaptive-precision-benchmarks.csv"))
        Writer.write(results.toCSVString(format = _.map(_.toString()).getOrElse("TIMEOUT"), rowName = "benchmark"))
        Writer.closeDefaultWriter()
