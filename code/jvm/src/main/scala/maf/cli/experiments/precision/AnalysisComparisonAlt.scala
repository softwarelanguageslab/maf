package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util.benchmarks._
import maf.util.{Reader, Writer}
import maf.language.scheme.primitives.SchemePrelude

import scala.concurrent.duration._
import maf.modular.scheme.modflocal.SchemeModFLocalAdaptiveWidening

abstract class AnalysisComparisonAlt[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the analyses to compare in terms of precision
    // - the number of runs for the concrete interpreter
    def analyses: List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 30min.) and the number of concrete runs
    def timeout() = Timeout.start(Duration(30, MINUTES)) // timeout for the analyses
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
          val t0 = System.nanoTime
          val otherResult = runAnalysis(analysis, name, program, path, timeout())
          val t1 = System.nanoTime
          val duration = (System.nanoTime - t0) / 1e9d
          println(s"duration: $duration")
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
    def k = 0
    def ls = List(100)
    lazy val modf: (SchemeExp => Analysis, String) = (SchemeAnalyses.kCFAAnalysis(_, k), s"$k-CFA MODF")
    lazy val dss: (SchemeExp => Analysis, String) = (SchemeAnalyses.modflocalAnalysis(_, k), s"$k-CFA DSS")
    lazy val wdss: (SchemeExp => Analysis, String) = (SchemeAnalyses.modFlocalAnalysisWidened(_, k), s"$k-CFA WDSS")
    lazy val dssFS: (SchemeExp => Analysis, String) = (SchemeAnalyses.modflocalFSAnalysis(_, k), s"$k-CFA DSS-FS")
    lazy val adaptive: List[(SchemeExp => Analysis, String)] = ls.map { l =>
      (SchemeAnalyses.modflocalAnalysisAdaptiveA(_, k, l), s"$k-CFA DSS w/ ASW (l = $l)")
    }
    def analyses = modf :: wdss :: dss :: dssFS :: adaptive
    def main0(args: Array[String]) = check("test/R5RS/gambit/matrix.scm")
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
        //"test/R5RS/gambit/matrix.scm",
        //"test/R5RS/gambit/mazefun.scm",
        //"test/R5RS/gambit/nqueens.scm",
        //"test/R5RS/gambit/peval.scm",
        //"test/R5RS/scp1/flatten.scm",
        //"test/R5RS/icp/icp_1c_multiple-dwelling.scm",
        //"test/R5RS/icp/icp_1c_ontleed.scm",
        //"test/R5RS/icp/icp_1c_prime-sum-pair.scm",
        //"test/R5RS/icp/icp_2_aeval.scm",
        //"test/R5RS/icp/icp_3_leval.scm",
        //"test/R5RS/icp/icp_5_regsim.scm",
        //"test/R5RS/icp/icp_7_eceval.scm",
        //"test/R5RS/icp/icp_8_compiler.scm",
        //"test/R5RS/various/lambda-update.scm",
        "test/R5RS/various/strong-update.scm"
      )
    )

    def check(path: String) =
        val prg = parseProgram(Reader.loadFile(path))
        var prv1: SchemeModFLocalAdaptiveWidening | Null = null
        val anl1: SchemeExp => Analysis = (prg: SchemeExp) =>
            prv1 = SchemeAnalyses.modflocalAnalysisAdaptiveA(prg, 0, 900)
            prv1.asInstanceOf[Analysis]
        var prv2: SchemeModFLocalAdaptiveWidening | Null = null
        val anl2: SchemeExp => Analysis = (prg: SchemeExp) =>
            prv2 = SchemeAnalyses.modflocalAnalysisAdaptiveA(prg, 0, 1000)
            prv2.asInstanceOf[Analysis]
        //val sel1: SchemeExp => Analysis = SchemeAnalyses.modFlocalAnalysisSelective(_, 0, prv1.nn.widened)
        //val sel2: SchemeExp => Analysis = SchemeAnalyses.modFlocalAnalysisSelective(_, 0, prv2.nn.widened)
        //val concrete = runInterpreter(prg, path, Timeout.none, runs).get
        val Terminated(res1) = runAnalysis(anl1, "adaptive analysis (l = 900)", prg, "test/R5RS/gambit/matrix.scm")
        val Terminated(res2) = runAnalysis(anl2, "selective analysis (l = 1000)", prg, "test/R5RS/gambit/matrix.scm")
        println(s"SUBSET?: ${prv1.nn.widened.subsetOf(prv2.nn.widened)} (${prv2.nn.widened.size} vs ${prv1.nn.widened.size})")

    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runBenchmarks(benchmarks: Set[Benchmark]) =
        benchmarks.foreach(runBenchmark)
        val cols = analyses.map(_._2)
        println(results.prettyString(columns = cols))
        Writer.setDefaultWriter(Writer.open("benchOutput/precision/precision-benchmarks.csv"))
        Writer.write(results.toCSVString(rowName = "benchmark", columns = cols))
        Writer.closeDefaultWriter()
