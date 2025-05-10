package maf.cli.experiments.precision

import maf.modular.scheme.aam._
import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util.benchmarks._
import maf.util.{Reader, Writer}
import maf.language.scheme.primitives.SchemePrelude

import scala.concurrent.duration._

abstract class AnalysisComparisonAlt[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the analyses to compare in terms of precision
    // - the number of runs for the concrete interpreter
    def analyses: List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 30min.) and the number of concrete runs
    def timeout() = Timeout.start(Duration(30, SECONDS)) // timeout for the analyses
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

    protected def additionalCounts(analysisName: String, path: Benchmark, r1: ResultMap, r2: ResultMap): Unit = ()

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
            val (lessPrecise, size) = otherResult match
                case Terminated(analysisResult) =>
                    additionalCounts(name, path, analysisResult, concreteResult)
                    (Result.Success(compareOrdered(analysisResult, concreteResult).size), Result.Success(analysisResult.keys.size))
                case TimedOut(partialResult) =>
                    additionalCounts(name, path, partialResult, concreteResult)
                    (Result.Timeout(compareOrdered(partialResult, concreteResult, check = false).size), Result.Timeout(partialResult.keys.size))
                case Errored(_) => (Result.Errored, Result.Errored)
            results = results.add(path, name, lessPrecise)
            results = results.add(path, s"$name-total", size)
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
    lazy val aam: (SchemeExp => Analysis, String) = (aamAnalysis(_, k), s"$k-CFA AAM")
    lazy val dss: (SchemeExp => Analysis, String)  = (SchemeAnalyses.modflocalAnalysis(_, k), s"$k-CFA DSS")
    //lazy val wdss: (SchemeExp => Analysis, String) = (SchemeAnalyses.modFlocalAnalysisWidened(_, k), s"$k-CFA WDSS")
    //lazy val dssFS: (SchemeExp => Analysis, String) = (SchemeAnalyses.modflocalFSAnalysis(_, k), s"$k-CFA DSS-FS")
    //lazy val adaptive: List[(SchemeExp => Analysis, String)] = ls.map { l =>
    //    (SchemeAnalyses.modflocalAnalysisAdaptiveA(_, k, l), s"$k-CFA DSS w/ ASW (l = $l)")
    //}

    def aamAnalysis(prg: SchemeExp, k: Int) = new SchemeAAMGCAnalysis(prg, k)

    def analyses = aam :: dss :: Nil

    def gambit = List("array1.scm",
                      "deriv.scm",
                     // "graphs.scm",
                      "nqueens.scm",
                      "puzzle.scm",
                      "sum.scm",
                      "triangl.scm",
                      "browse.scm",
                      "destruc.scm",
                      "lattice.scm",
                      "paraffins.scm",	
                      "sboyer.scm",	
                      "sumloop.scm",
                      "wc.scm",
                      "cat.scm",
                      "diviter.scm",
                      "matrix.scm",	
                      "perm9.scm",	
                      "scheme.scm",	
                      "tail.scm",
                      "compiler.scm",
                      "earley.scm",	
                      "mazefun.scm",	
                      "peval.scm",	
                      "slatex.scm",	
                      "tak.scm",
                      "ctak.scm",	
                      "fibc.scm",	
                      "nboyer.scm",	
                      "primes.scm",	
                      "string.scm",	
                      "trav1.scm"
                    ).map(file => s"test/R5RS/various/$file")

    def gabriel = List(
      "boyer",
      "browse",
      "cpstak",
      "dderiv",
      "deriv",
      "destruct",
      "diviter",
      "divrec",
      "puzzle",
      "takl",
      "triangl"
    ).map(name => s"test/R5RS/gabriel/$name.scm")

    def main(args: Array[String]) = runBenchmarks(gabriel)
    /*
      List(
        //VARIOUS
        "test/R5RS/various/collatz.scm",
        "test/R5RS/various/mceval.scm",
        "test/R5RS/various/church.scm",
        "test/R5RS/various/regex.scm",
        "test/R5RS/various/blur.scm",
        "test/R5RS/various/bound-precision.scm",
        "test/R5RS/various/eta.scm",
        "test/R5RS/various/gcipd.scm",
        "test/R5RS/various/four-in-a-row.scm",
        "test/R5RS/various/grid.scm",
        "test/R5RS/various/mj09.scm",
        "test/R5RS/various/primtest.scm",
        "test/R5RS/various/rsa.scm",
        //GAMBIT
        "test/R5RS/gambit/array1.scm",
        "test/R5RS/gambit/deriv.scm",
        "test/R5RS/gambit/tak.scm",
        "test/R5RS/gambit/array1.scm",
        "test/R5RS/gambit/destruc.scm",
        "test/R5RS/gambit/diviter.scm",
        "test/R5RS/gambit/lattice.scm",
        "test/R5RS/gambit/nboyer.scm",
        "test/R5RS/gambit/paraffins.scm",
        "test/R5RS/gambit/perm9.scm",
        "test/R5RS/gambit/perm.scm",
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
      */

    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runBenchmarks(benchmarks: List[Benchmark]) =
        assert(benchmarks.size == benchmarks.toSet.size)
        benchmarks.foreach(runBenchmark)
        val cols = analyses.map(_._2)
        println(results.prettyString(columns = cols))
        val writer = Writer.open("benchOutput/precision/precision-benchmarks.csv")
        Writer.write(writer, results.toCSVString(rowName = "benchmark", columns = cols))
        Writer.close(writer)
