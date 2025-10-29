package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util._
import maf.util.benchmarks._

import scala.concurrent.duration._

abstract class AdaptiveAnalysisComparison[
    Num: IntLattice,
    Rea: RealLattice,
    Bln: BoolLattice,
    Chr: CharLattice,
    Str: StringLattice,
    Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - a timeout and number of concrete runs
    def timeout = Duration(30, MINUTES) // timeout for the analyses
    def concreteRuns = 3
    // - a list of non-adaptive analysis and adaptive analyses (with increasing precision)
    def baseAnalyses: List[(SchemeExp => Analysis, String)]
    def adaptiveAnalyses: List[(SchemeExp => Analysis, String)]

    // keep the results of the benchmarks in a table
    var results: Table[Option[Int]] = Table.empty.withDefaultValue(None)

    protected def compareUntilTimeout(
        analyses: List[(SchemeExp => Analysis, String)],
        path: String,
        program: SchemeExp,
        concreteResult: ResultMap
      ): Unit =
        analyses.foreach { case (analysis, name) =>
            val start = System.nanoTime()
            runAnalysis(analysis, name, program, path, Timeout.start(timeout)) match
                case Terminated(store) =>
                    val lessPrecise = compareOrdered(store, concreteResult).size
                    val end = System.nanoTime()
                    val duration = (System.nanoTime() - start).nanos.toMillis
                    val time = Some(duration.toInt)
                    results = results.add(path, name, Some(lessPrecise))
                    results = results.add(path, name ++ "(ms)", time)
                case _ => return // don't run the other analyses anymore
        }

    /**
     * For a given benchmark, compare non-adaptive analyses with adaptive ones To do so, two results are included per benchmark
     *   - the most precise non-adaptive analysis that terminated within
     *   - the most precise adaptive analysis that terminated All results are saved in the `result` table of this object
     *
     * @param path
     *   the name of / path to the benchmark program
     * @param program
     *   the Scheme expression of the benchmark program
     */
    protected def forBenchmark(path: Benchmark, program: SchemeExp) =
        // run the concrete interpreter analysis first
        val concreteResult = runInterpreter(program, path, Timeout.none, concreteRuns).get // no timeout set for the concrete interpreter
        // find the most precise non-adaptive analysis
        compareUntilTimeout(baseAnalyses, path, program, concreteResult)
        // find the most precise adaptive analysis
        compareUntilTimeout(adaptiveAnalyses, path, program, concreteResult)

    protected def runBenchmarks(benchmarks: List[Benchmark]) =
      assert(benchmarks.size == benchmarks.toSet.size) // sanity check to make sure we don't have duplicates
      benchmarks.foreach { benchmark =>
        runBenchmark(benchmark)
      }
      showResults()
    
    protected def showResults() =
      println()
      println("RESULTS")
      println("=================")
      println()
      println(results.prettyString())

object SelectiveAnalysisComparison extends AdaptiveAnalysisComparison[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:

    def findKs(path: Benchmark, program: SchemeExp) = 
        var analysis = SchemeAnalyses.charachteristicsAnalysis(program)
        analysis.analyze()
        val functions = analysis.functions
        var result = Map.empty[String, Int]
        for (f <- analysis.functions)
            result = result + (f.functionName -> 1)
        println(result)
        result

    override def forBenchmark(path: Benchmark, program: SchemeExp) =
        // run the concrete interpreter analysis first
        val concreteResult = runInterpreter(program, path, Timeout.none, concreteRuns).get // no timeout set for the concrete interpreter
        // find the most precise non-adaptive analysis
        compareUntilTimeout(baseAnalyses, path, program, concreteResult)

        // find the k's for this program 
        val ks = findKs(path, program)
        
        // find the most precise adaptive analysis
        compareUntilTimeout(List((SchemeAnalyses.selectiveKCFAAnalysis(_, ks), "selective")), path, program, concreteResult)

    override def concreteRuns = 1

    override def baseAnalyses = List((SchemeAnalyses.contextInsensitiveAnalysis(_), "0 kcfa"),
                                     (SchemeAnalyses.kCFAAnalysis(_, 1), "1 kcfa"))
    override def adaptiveAnalyses = List((SchemeAnalyses.selectiveKCFAAnalysis(_, Map.empty), "selective"))

    def gabrielBenchmarks = 
      List(
        "boyer",
        "browse",
        "cpstak",
        "dderiv",
        "deriv",
        "destruc",
        "diviter",
        "divrec",
        "takl",
        //"triangl" <- times out in concrete interpreter
      ).map(name => s"test/R5RS/gabriel/$name.scm")

    def benchmarks = List("test/R5RS/various/example.scm")

    def main(args: Array[String]): Unit = {
        MAFLogger.disable()
        runBenchmarks(benchmarks)
   }

