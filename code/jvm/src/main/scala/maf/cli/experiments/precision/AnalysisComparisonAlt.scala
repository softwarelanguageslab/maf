package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util.benchmarks._
import maf.util.Writer

import scala.concurrent.duration._

abstract class AnalysisComparisonAlt[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks {

  // the precision comparison is parameterized by:
  // - the analyses to compare in terms of precision
  // - the number of runs for the concrete interpreter
  def analyses: List[(SchemeExp => Analysis, String)]

  // and can, optionally, be configured in its timeouts (default: 30min.) and the number of concrete runs
  def timeout() = Timeout.start(Duration(30, MINUTES)) // timeout for the analyses
  def runs = 3 // number of runs for the concrete interpreter

  // keep the results of the benchmarks in a table
  var results: Table[Option[Int]] = Table.empty[Option[Int]]

  /**
   * For a given benchmark, compare each analysis with the result of the concrete interpreter
   * That is, count for each analysis how many values are strictly over-approximating the result of the concrete interpreter
   * All results are saved in the `result` table of this object
   *
   * @param path the name of / path to the benchmark program
   * @param program the Scheme expression of the benchmark program
   */
  protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit = {
    // run the concrete interpreter analysis first
    val concreteResult = runInterpreter(program, path, Timeout.none, runs).get // no timeout set for the concrete interpreter
    // run the other analyses on the benchmark
    analyses.foreach { case (analysis, name) =>
      val otherResult = runAnalysis(analysis, name, program, path, timeout())
      val lessPrecise = otherResult.map(store => compareOrdered(store, concreteResult).size)
      results = results.add(path, name, lessPrecise)
    }
  }
}

object AnalysisComparisonAltAdaptive
    extends AnalysisComparisonAlt[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ] {
  def analyses = {
    val n = 3000
    val t = 3000
    // run some adaptive analyses
    List((SchemeAnalyses.adaptiveAnalysis(_, n, t), s"adaptive (n = $n; t = $t)"))
  }
  def main(args: Array[String]) = runBenchmarks(
    Set(
      "test/R5RS/various/mceval.scm",
      "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
      "test/R5RS/icp/icp_2_aeval.scm",
      //"test/R5RS/icp/icp_7_eceval.scm",
      "test/R5RS/icp/icp_8_compiler.scm",
      "test/R5RS/gabriel/browse.scm",
      "test/R5RS/gabriel/boyer.scm",
      "test/R5RS/scp1-compressed/all.scm"
    )
  )

  def runBenchmarks(benchmarks: Set[Benchmark]) = {
    benchmarks.foreach(runBenchmark)
    println(results.prettyString(format = _.map(_.toString()).getOrElse("TIMEOUT")))
    Writer.setDefaultWriter(Writer.open("benchOutput/precision/adaptive-precision-benchmarks-alt.csv"))
    Writer.write(results.toCSVString(format = _.map(_.toString()).getOrElse("TIMEOUT"), rowName = "benchmark"))
    Writer.closeDefaultWriter()
  }
}
