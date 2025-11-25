package maf.cli.experiments.precision

import maf.cli.experiments._
import maf.language.scheme._
import maf.lattice._
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.util._
import maf.util.benchmarks._
import maf.bench.scheme._

import scala.concurrent.duration._

abstract class AdaptiveAnalysisComparison[
    Num: IntLattice,
    Rea: RealLattice,
    Bln: BoolLattice,
    Chr: CharLattice,
    Str: StringLattice,
    Smb: SymbolLattice]
    extends PrecisionBenchmarks[Num, Rea, Bln, Chr, Str, Smb]: //with ResultsPerCmp[Num, Rea, Bln, Chr, Str, Smb]:

    // the precision comparison is parameterized by:
    // - a timeout and number of concrete runs and whether or not to time the analyses
    def timeout = Duration(30, MINUTES) // timeout for the analyses
    def concreteRuns = 3
    def timed = true
    // - a list of non-adaptive analysis and adaptive analyses (with increasing precision)
    def baseAnalyses: List[(SchemeExp => Analysis, String)]
    def adaptiveAnalyses: List[(SchemeExp => Analysis, String)]


    // keep the results of the benchmarks in a table
    var results: Table[Option[String]] = Table.empty.withDefaultValue(None)

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
                    val compared = compareOrdered(store, concreteResult, check=false)
                    // println(compared)
                    val lessPrecise = compared.size
                    val end = System.nanoTime()
                    val duration = (System.nanoTime() - start).nanos.toMillis
                    results = results.add(path, name, Some(lessPrecise.toString))
                    // results = results.add(path, name ++ " ids", Some(compared.toString.replace(",", "").nn)) // add the set of less precise identifiers
                    if (timed) then results = results.add(path, name ++ "(ms)", Some(duration.toString))
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
        val concreteResult = runInterpreter(program, path, Timeout.start(timeout), concreteRuns)

        concreteResult match 
          case None => return
          case Some(v) =>
            // find the most precise non-adaptive analysis
            compareUntilTimeout(baseAnalyses, path, program, v)
            // find the most precise adaptive analysis
            compareUntilTimeout(adaptiveAnalyses, path, program, v)
            // write the results to a file
            val csv = results.toCSVString(rows = List(path), format = _.map(_.toString()).getOrElse("TIMEOUT"))
            writeToFile(csv, benchmarkFolder ++ path.stripPrefix("test/R5RS/").stripSuffix(".scm") ++ ".csv")

    override protected def addPreviousBenchmarkResult(path: String) = 
      val csv = Reader.loadFile(path)
      val table: Table[Option[String]] = Table.fromCSVString(csv, Some(_))
      results = Table.append(results, table)

    protected def writeToFile(output: String, path: String) = 
      val writer = Writer.open(path)
      Writer.write(writer, output)
      Writer.close(writer)

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

    def benchmarkFolder = "out/selective/"

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
        val concreteResult = runInterpreter(program, path, Timeout.start(timeout), concreteRuns)

        concreteResult match 
          case None => return
          case Some(v) =>
            // find the most precise non-adaptive analysis
            compareUntilTimeout(baseAnalyses, path, program, v)
            // find the k's for this program 
            val ks = findKs(path, program) 
            // find the most precise adaptive analysis
            compareUntilTimeout(List((SchemeAnalyses.selectiveKCFAAnalysis(_, ks), "selective")), path, program, v)

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



object AdaptiveContextSensitivityAnalysisComparison
    extends AdaptiveAnalysisComparison[
      ConstantPropagation.I,
      ConstantPropagation.R,
      ConstantPropagation.B,
      ConstantPropagation.C,
      ConstantPropagation.S,
      ConstantPropagation.Sym
    ]:

    def benchmarkFolder = "out/adaptive/"

    override def timeout = Duration(30, MINUTES)

    override def concreteRuns = 1

    override def baseAnalyses = List(
                                    // (SchemeAnalyses.contextInsensitiveAnalysis(_), "0 kcfa"),
                                    // (SchemeAnalyses.kCFAAnalysis(_, 1), "1 kcfa"),
                                    // (SchemeAnalyses.kCFAAnalysis(_, 2), "2 kcfa"),
                                    // (SchemeAnalyses.kCFAAnalysis(_, 3), "3 kcfa")
                                     )
    override def adaptiveAnalyses = List(
      // (SchemeAnalyses.standardAdaptiveContextSensitiveAnalysis(_), "select: most contexts"),
      // (SchemeAnalyses.fullyRandomAdaptiveContextSensitiveAnalysis(_), "fully random"),                  
      // (SchemeAnalyses.selectRandomAdaptiveContextSensitiveAnalysis(_), "select: random"),                  
      // (SchemeAnalyses.selectMostDependenciesAdaptiveContextSensitiveAnalysis(_), "select: most dependencies"),                  
      // (SchemeAnalyses.selectLeastDependenciesAdaptiveContextSensitiveAnalysis(_), "select: least dependencies"),                  
      // (SchemeAnalyses.selectMostContextsLeastDependenciesAdaptiveContextSensitiveAnalysis(_), "select: combined"),                  
      (SchemeAnalyses.selectDifferentValuesAdaptiveContextSensitiveAnalysis(_), "select: different values"),                  
      // (SchemeAnalyses.selectImpreciseAdaptiveContextSensitiveAnalysis(_), "select: imprecise"),                  
      // (SchemeAnalyses.randomAdaptiveContextSensitiveAnalysis(_), "when: random"),                  
      // (SchemeAnalyses.alwaysAdaptiveContextSensitiveAnalysis(_), "when: always")             
      )    

    def variousBenchmarks: List[String] = List(
          "test/R5RS/WeiChenRompf2019/meta-circ.scm",
          "test/R5RS/WeiChenRompf2019/earley.sch",
          "test/R5RS/WeiChenRompf2019/toplas98/graphs.scm",
          "test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm",
          "test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm",
          // "test/R5RS/WeiChenRompf2019/toplas98/boyer.scm", // concrete times out
          "test/R5RS/gabriel/boyer.scm",
          "test/R5RS/gambit/peval.scm",
          "test/R5RS/gambit/scheme.scm",
          // "test/R5RS/gambit/sboyer.scm",
          // "test/R5RS/gambit/nboyer.scm",
          "test/R5RS/gambit/matrix.scm",
          "test/R5RS/gambit/browse.scm",
          // "test/R5RS/scp1-compressed/all.scm",
          "test/R5RS/ad/all.scm",
          "test/R5RS/various/SICP-compiler.scm",
          "test/R5RS/icp/icp_1c_ambeval.scm",
          "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
          "test/R5RS/icp/icp_1c_ontleed.scm",
          "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
          "test/R5RS/icp/icp_7_eceval.scm",
          "test/R5RS/icp/icp_8_compiler.scm",
          "test/R5RS/icp/icp_5_regsim.scm",
          "test/R5RS/icp/icp_3_leval.scm",
          "test/R5RS/icp/icp_2_aeval.scm",
        )

    def gabrielBenchmarks: List[String] = 
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

    def interestingBenchmarks: List[String] = List(
      "test/R5RS/gambit/browse.scm",
      "test/R5RS/gambit/matrix.scm",
      "test/R5RS/icp/icp_8_compiler.scm",
      "test/R5RS/various/SICP-compiler.scm",
      "test/R5RS/gabriel/boyer.scm",
      "test/R5RS/gabriel/browse.scm",
    )


    def benchmarks = interestingBenchmarks
    // def benchmarks = variousBenchmarks
    // def benchmarks = gabrielBenchmarks

    def main(args: Array[String]): Unit = {
        MAFLogger.disable()
        runBenchmarks(benchmarks)
        val csv = results.toCSVString(rowName = "benchmark", format = _.map(_.toString()).getOrElse("TIMEOUT"))
        writeToFile(csv, benchmarkFolder.stripSuffix("/") ++ ".csv")
    }