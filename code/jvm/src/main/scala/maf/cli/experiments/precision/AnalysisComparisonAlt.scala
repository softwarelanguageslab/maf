package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.modular.AnalysisEntry
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util.benchmarks._
import maf.util.{Reader, Writer}
import maf.language.scheme.primitives.SchemePrelude
import maf.bench.scheme.*
import scala.concurrent.duration._
import maf.modular.scheme.modflocal._


//compares analyses by counting number of over-approximations w.r.t. a concrete interpreter
//(lower is better)
abstract class AnalysisComparisonAlt[Num: IntLattice, Rea: RealLattice, Bln: BoolLattice, Chr: CharLattice, Str: StringLattice, Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - the analyses to compare in terms of precision
    // - the benchmarks to compare the analyses on
    // - the number of runs for the concrete interpreter
    def analyses: List[(SchemeExp => Analysis, String)]
    def benchmarks: List[Benchmark]
    def runs = 3 // number of runs for the concrete interpreter

    // and can, optionally, be configured in its timeouts (default: 10min.) and the number of concrete runs
    def timeout() = Timeout.start(Duration(10, MINUTES)) // timeout for the analyses

    // keep the results of the benchmarks in a table
    enum Result:
        case Success(abs: Int)
        case Timeout(abs: Int)
        case Errored
        override def toString = this match
            case Result.Success(abs) => s"$abs"
            case Result.Timeout(abs) => s"TIMEOUT (>= $abs)"
            case Result.Errored      => "ERROR"

    var precisionResults:   Table[Result] = Table.empty
    var performanceResults: Table[Option[Double]] = Table.empty 

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
            val duration = (System.nanoTime - t0) / 1e9d  //duration is reported in seconds
            println(s"duration: $duration")
            val (lessPrecise, size) = otherResult match
                case Terminated(analysisResult) =>
                    additionalCounts(name, path, analysisResult, concreteResult)
                    performanceResults = performanceResults.add(path, name, Some(duration))
                    (Result.Success(compareOrdered(analysisResult, concreteResult).size), Result.Success(analysisResult.keys.size))
                case TimedOut(partialResult) =>
                    additionalCounts(name, path, partialResult, concreteResult)
                    performanceResults = performanceResults.add(path, name, None)
                    (Result.Timeout(compareOrdered(partialResult, concreteResult, check = false).size), Result.Timeout(partialResult.keys.size))
                case Errored(_) => 
                    performanceResults = performanceResults.add(path, name, None)
                    (Result.Errored, Result.Errored)
            precisionResults = precisionResults.add(path, name, lessPrecise)
            //precisionResults = precisionResults.add(path, s"$name-total", size)
        }

    private def runBenchmarks(benchmarks: List[Benchmark], outputFolder: Option[String] = None) =
      assert(benchmarks.size == benchmarks.toSet.size) // sanity check to make sure we don't have duplicates
      benchmarks.foreach { benchmark =>
        runBenchmark(benchmark)
        writeResultsToFile(outputFolder)
      }
      showResults()
    
    protected def showResults() =
      println()
      println("PRECISION RESULTS")
      println("=================")
      println()
      println(precisionResults.prettyString())
      println()
      println("PERFORMANCE RESULTS")
      println("===================")
      println()
      println(performanceResults.prettyString())

    protected def writeToFile[X](results: Table[X], path: String) =
      val writer = Writer.open(path)
      Writer.write(writer, results.toCSVString(rowName = "benchmark"))
      Writer.close(writer)

    protected def writeResultsToFile(outputFolder: Option[String]) =
      outputFolder.foreach { dir => 
        writeToFile(precisionResults,   s"$dir/precision-benchmarks.csv")
        writeToFile(performanceResults, s"$dir/performance-benchmarks.csv")      
      }

    def main(args: Array[String]) = runBenchmarks(benchmarks, args.headOption)


class SingleBenchmark(analysis: AnalysisEntry[SchemeExp], benchmark: String) 
  extends AnalysisComparisonAlt[
    ConstantPropagation.I,
    ConstantPropagation.R,
    ConstantPropagation.B,
    ConstantPropagation.C,
    ConstantPropagation.S,
    ConstantPropagation.Sym]:

  def analyses = List((((_: SchemeExp) => analysis.asInstanceOf[Analysis]), analysis.toString()))
  // def analyses = List((SchemeAnalyses.kCFAAnalysis(_, k), s"$k-CFA analysis"))
  def benchmarks = List(benchmark)

  override protected def showResults() =
      println()
      println("PRECISION RESULTS")
      println(precisionResults.get(benchmark, analysis.toString())getOrElse("n/a"))
      println()
      println("PERFORMANCE RESULTS")
      println(performanceResults.get(benchmark, analysis.toString()).flatten.getOrElse("n/a"))

abstract class SASBenchmarks
    extends AnalysisComparisonAlt[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:

    lazy val aam0: (SchemeExp => Analysis, String)        = (SchemeAnalyses.aamGCAnalysis(_, 0),              "0-CFA AAM")
    lazy val aam1: (SchemeExp => Analysis, String)        = (SchemeAnalyses.aamGCAnalysis(_, 1),              "1-CFA AAM")
    lazy val aam2: (SchemeExp => Analysis, String)        = (SchemeAnalyses.aamGCAnalysis(_, 2),              "2-CFA AAM")
    lazy val dss0: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modflocalAnalysis(_, 0),          "0-CFA DSS")
    lazy val dss1: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modflocalAnalysis(_, 1),          "1-CFA DSS")
    lazy val dss2: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modflocalAnalysis(_, 2),          "2-CFA DSS")
    lazy val adi0: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modfADIAnalysis(_, 0),            "0-CFA ADI")
    lazy val adi1: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modfADIAnalysis(_, 1),            "1-CFA ADI")
    lazy val adi2: (SchemeExp => Analysis, String)        = (SchemeAnalyses.modfADIAnalysis(_, 2),            "2-CFA ADI")
    lazy val dssFS0: (SchemeExp => Analysis, String)      = (SchemeAnalyses.modflocalFSAnalysis(_, 0, true),  "0-CFA DSS-FS")
    lazy val dssFS1: (SchemeExp => Analysis, String)      = (SchemeAnalyses.modflocalFSAnalysis(_, 1, true),  "1-CFA DSS-FS")
    lazy val dssFS2: (SchemeExp => Analysis, String)      = (SchemeAnalyses.modflocalFSAnalysis(_, 2, true),  "2-CFA DSS-FS")
    lazy val dssFS0NoGC: (SchemeExp => Analysis, String)  = (SchemeAnalyses.modflocalFSAnalysis(_, 0, false), "0-CFA DSS-FS (without GC)")
    lazy val dssFS1NoGC: (SchemeExp => Analysis, String)  = (SchemeAnalyses.modflocalFSAnalysis(_, 1, false), "1-CFA DSS-FS (without GC)")
    lazy val dssFS2NoGC: (SchemeExp => Analysis, String)  = (SchemeAnalyses.modflocalFSAnalysis(_, 2, false), "2-CFA DSS-FS (without GC)")

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

    def extraBenchmarks = 
      List(
          "test/R5RS/various/grid.scm",
          "test/R5RS/gambit/matrix.scm",
          "test/R5RS/various/mceval.scm",
          "test/R5RS/various/regex.scm",
          "test/R5RS/WeiChenRompf2019/rsa.scm",
          "test/R5RS/gambit/tak.scm",
      )

    def benchmarks = gabrielBenchmarks ++ extraBenchmarks

    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)

object AllSASBenchmarks extends SASBenchmarks:
  def analyses = List(
    aam0, aam1, aam2,
    dss0, dss1, dss2,
    dssFS0, dssFS1, dssFS2,
    dssFS0NoGC, dssFS1NoGC, dssFS2NoGC
  )

object Table2Benchmarks extends SASBenchmarks:
  def analyses = List(
    aam0, aam1, aam2,
    dss0, dss1, dss2,
)

object Table3Benchmarks extends SASBenchmarks:
  def analyses = List(
    dss0, dss1, dss2,
    dssFS0, dssFS1, dssFS2,
)

object Table4Benchmarks extends SASBenchmarks:
  def analyses = List(
    dssFS0, dssFS1, dssFS2,
    dssFS0NoGC, dssFS1NoGC, dssFS2NoGC
  )

object ADIBenchmarks extends SASBenchmarks:
  def analyses = List(
    dss0, dss1, dss2,
    adi0, adi1, adi2
  )

object DSS2Benchmark extends SASBenchmarks:
  def analyses = List(dssFS2NoGC)
  override def benchmarks = List("test/R5RS/gabriel/boyer.scm")

object CountIterationBenchmark extends SASBenchmarks:

  def analyses = List(
    dssFS0, dssFS0NoGC
  )

  var results: Table[Int] = Table.empty 

  override protected def forBenchmark(path: Benchmark, program: SchemeExp): Unit =
    println(s"ANALYZING $path")
    analyses.foreach { case (analysis, name) => 
      val anl = analysis(program)
      anl.analyzeWithTimeout(Timeout.start(Duration(15, MINUTES)))
      results = results.add(path, name, anl.asInstanceOf[SchemeModFLocalFS].iterations)
    }

  override def timeout() = Timeout.none

  override protected def showResults() =
    println(results.prettyString())