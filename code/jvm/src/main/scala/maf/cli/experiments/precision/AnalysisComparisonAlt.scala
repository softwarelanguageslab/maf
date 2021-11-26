package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util.benchmarks._
import maf.util.Writer
import maf.language.scheme.primitives.SchemePrelude

import scala.concurrent.duration._

abstract class AnalysisComparisonAlt[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the analyses to compare in terms of precision
    // - the number of runs for the concrete interpreter
    def analyses: List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 30min.) and the number of concrete runs
    def timeout() = Timeout.start(Duration(60, MINUTES)) // timeout for the analyses
    def runs = 3 // number of runs for the concrete interpreter

    // keep the results of the benchmarks in a table
    enum Result:
        case Success(abs: Int)
        case Timeout(abs: Int)
        case Errored
        override def toString = this match
            case Result.Success(abs) => s"$abs"
            case Result.Timeout(abs) => s"TIMEOUT (>= $abs)"
            case Result.Errored      => "ERROR"
    var results: Table[Result] = Table.empty

    /**
     * For a given benchmark, compare each analysis with the result of the concrete interpreter That is, count for each analysis how many values are
     * strictly over-approximating the result of the concrete interpreter All results are saved in the `result` table of this object
     *
     * @param path
     *   the name of / path to the benchmark program
     * @param program
     *   the Scheme expression of the benchmark program
     */
    protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit =
        // run the concrete interpreter analysis first
        val concreteResult = runInterpreter(program, path, Timeout.none, runs).get // no timeout set for the concrete interpreter
        // run the other analyses on the benchmark
        analyses.foreach { case (analysis, name) =>
          val otherResult = runAnalysis(analysis, name, program, path, timeout())
          val lessPrecise = otherResult match
              case Terminated(analysisResult) => Result.Success(compareOrdered(analysisResult, concreteResult).size)
              case TimedOut(partialResult)    => Result.Timeout(compareOrdered(partialResult, concreteResult, check = false).size)
              case Errored(_)                 => Result.Errored
          results = results.add(path, name, lessPrecise)
        }

object AnalysisComparisonAlt1
    extends AnalysisComparisonAlt[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:
    def analyses =
        val k = 0
        val ls = List(100, 200, 300, 400)
        // run some adaptive analyses
        //(SchemeAnalyses.modflocalAnalysis(_, 0), "0-CFA DSS") ::
        //(SchemeAnalyses.kCFAAnalysis(_, k), s"$k-CFA MODF") ::
        val default: List[(SchemeExp => Analysis, String)] = List(
          (SchemeAnalyses.modflocalAnalysis(_, k), s"$k-CFA DSS"),
          (SchemeAnalyses.kCFAAnalysis(_, k), s"$k-CFA MODF")
        )
        val adaptive: List[(SchemeExp => Analysis, String)] = ls.map { l =>
          (SchemeAnalyses.modflocalAnalysisAdaptive(_, k, l), s"$k-CFA DSS w/ ASW (l = $l)"),
        }
        default ++ adaptive
    def main(args: Array[String]) = runBenchmarks(
      Set(
        //"test/R5RS/various/collatz.scm",
        //"test/R5RS/various/mceval.scm",
        //"test/R5RS/various/church.scm",
        //"test/R5RS/various/regex.scm",
        //"test/R5RS/various/blur.scm",
        //"test/R5RS/various/bound-precision.scm",
        //"test/R5RS/various/eta.scm",
        //"test/R5RS/various/gcipd.scm",
        //"test/R5RS/various/four-in-a-row.scm",
        //"test/R5RS/various/grid.scm",
        //"test/R5RS/various/mj09.scm",
        //"test/R5RS/various/primtest.scm",
        //"test/R5RS/various/rsa.scm",
        //"test/R5RS/gambit/deriv.scm",
        //"test/R5RS/gambit/tak.scm",
        //"test/R5RS/gambit/browse.scm",
        //"test/R5RS/gambit/earley.scm",
        "test/R5RS/gambit/matrix.scm",
        "test/R5RS/gambit/mazefun.scm",
        //"test/R5RS/gambit/nqueens.scm",
        //"test/R5RS/gambit/peval.scm",
      )
    )

    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runBenchmarks(benchmarks: Set[Benchmark]) =
        benchmarks.foreach(runBenchmark)
        println(results.prettyString())
        Writer.setDefaultWriter(Writer.open("benchOutput/precision/precision-benchmarks.csv"))
        Writer.write(results.toCSVString(rowName = "benchmark"))
        Writer.closeDefaultWriter()
