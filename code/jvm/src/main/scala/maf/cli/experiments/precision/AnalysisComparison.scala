package maf.cli.experiments.precision

import maf.modular.scheme.aam._
import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util._
import maf.util.benchmarks._
import maf.language.scheme.primitives.SchemePrelude

import scala.concurrent.duration._

abstract class AnalysisComparison[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the base analysis (= lowest precision) to compare to
    // - the other analyses to compare to the base analysis
    def baseAnalysis(prg: SchemeExp): Analysis
    def otherAnalyses(): List[(SchemeExp => Analysis, String)]

    // and can, optionally, be configured in its timeouts (default: 5min.)
    def analysisTimeout(): Timeout.T = Timeout.start(Duration(30, SECONDS)) //timeout for (non-base) analyses
    def concreteTimeout(): Timeout.T = Timeout.start(Duration(30, SECONDS)) //timeout for concrete interpreter

    def concreteRuns() = 3

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
    def benchmarkFolder = "out/analysiscomparison1/"

    val k = 0

    lazy val aam: (SchemeExp => Analysis, String) = (new SchemeAAMGCAnalysis(_, k),          s"$k-CFA AAM")
    lazy val dss: (SchemeExp => Analysis, String) = (SchemeAnalyses.modflocalAnalysis(_, k), s"$k-CFA DSS")

    def baseAnalysis(prg: SchemeExp): Analysis =
        SchemeAnalyses.contextInsensitiveAnalysis(prg) //context-insensitive analysis
    def otherAnalyses() = aam :: dss :: Nil 

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
        "destruc",
        "diviter",
        "divrec",
        "puzzle",
        "takl",
        "triangl"
     ).map(name => s"test/R5RS/gabriel/$name.scm")

    def sas2025 = 
        List(
            "test/R5RS/gambit/deriv.scm",
            "test/R5RS/gambit/tak.scm",
            "test/R5RS/various/grid.scm",
            "test/R5RS/various/regex.scm",
            "test/R5RS/various/rsa.scm"
        )


    def main(args: Array[String]) = runBenchmarks(sas2025)

    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

    def runBenchmarks(benchmarks: List[Benchmark]) =
        assert(benchmarks.size == benchmarks.toSet.size)
        benchmarks.foreach(runBenchmark)
        println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
        val writer = Writer.open("benchOutput/precision/precision-benchmarks.csv")
        Writer.write(writer, results.toCSVString(format = _.map(_.toString()).getOrElse("TIMEOUT"), rowName = "benchmark"))
        Writer.close(writer)
