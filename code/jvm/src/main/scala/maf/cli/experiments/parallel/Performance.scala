package maf.cli.experiments.parallel

import maf.language.scheme._
import maf.cli.experiments._
import maf.cli.experiments.performance._
import maf.util.benchmarks._
import scala.concurrent.duration._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.modular.worklist.LeastVisitedFirstWorklistAlgorithm
import maf.modular.worklist.LeastVisitedFirstWorklistAlgorithm
import maf.modular.worklist.DeepExpressionsFirstWorklistAlgorithm
import maf.modular.worklist.LeastDependenciesFirstWorklistAlgorithm
import maf.util._
import maf.modular.scheme.modflocal._
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.worklist.MostVisitedFirstWorklistAlgorithm
import maf.modular.worklist.RandomWorklistAlgorithm
import maf.modular.worklist.MostVisitedFirstWorklistAlgorithm
import maf.modular.worklist.CallDepthFirstWorklistAlgorithm
import maf.modular.worklist.CallDepthFirstWorklistAlgorithm

object ParallelDSSAnalyses:

    abstract class ParallelDSSAnalysis(prg: SchemeExp, n: Int, kcfa: Int) 
        extends SchemeModFLocal(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(kcfa)
            with ParallelWorklistAlgorithm[SchemeExp] {

        type AnalysisState = Map[Component, Set[(Val, Dlt, Set[Adr], Set[Adr])]]
        def analysisState = results
    
        override def workers = n 
        override def intraAnalysis(cmp: Component) = 
            new SchemeLocalIntraAnalysis(cmp) with ParallelIntra { intra => 
                override def setLocalState(st: AnalysisState) = () //intra.results = st
            }
    }

    abstract class ParallelDSSFSAnalysis(prg: SchemeExp, n: Int, kcfa: Int) 
        extends SchemeModFLocalFS(prg)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalCallSiteSensitivity(kcfa)
            with ParallelWorklistAlgorithm[SchemeExp] {

        type AnalysisState = Unit
        def analysisState = ()
    
        override def workers = n 
        override def intraAnalysis(cmp: Component) = 
            new SchemeModFLocalFSIntraAnalysis(cmp) with ParallelIntra { intra => 
                override def setLocalState(st: AnalysisState) = ()
            }
    }

    def parallelDSS(prg: SchemeExp, n: Int, kcfa: Int) = 
        new ParallelDSSAnalysis(prg, n, kcfa) with CallDepthFirstWorklistAlgorithm[SchemeExp]

    def parallelDSSFS(prg: SchemeExp, n: Int, kcfa: Int) = 
        new ParallelDSSFSAnalysis(prg, n, kcfa) with CallDepthFirstWorklistAlgorithm[SchemeExp]

object ParallelModFAnalyses:

    abstract class ParallelModFAnalysis(prg: SchemeExp, n: Int, kcfa: Int)
        extends ModAnalysis(prg) with SchemeModFSemanticsM
                                 with StandardSchemeModFComponents
                                 with BigStepModFSemantics
                                 with ParallelWorklistAlgorithm[SchemeExp]
                                 with SchemeModFKCallSiteSensitivity
                                 with SchemeConstantPropagationDomain:

        type AnalysisState = Map[Addr, Value]
        def analysisState = store 
        
        override val k = kcfa
        override def workers = n 
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra: 
            intra => 
            override def setLocalState(st: AnalysisState) = intra.store = st 

object ParallelBenchmarks:
    def paperName: Map[String, String] = List(
      ("test/R5RS/WeiChenRompf2019/meta-circ.scm", "meta-circ"),
      ("test/R5RS/WeiChenRompf2019/earley.sch", "earley"),
      ("test/R5RS/WeiChenRompf2019/toplas98/graphs.scm", "graphs"),
      ("test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm", "dynamic"),
      ("test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm", "nbody"),
      ("test/R5RS/WeiChenRompf2019/toplas98/boyer.scm", "boyer"),
      ("test/R5RS/gambit/peval.scm", "peval"),
      ("test/R5RS/gambit/scheme.scm", "scheme"),
      ("test/R5RS/gambit/sboyer.scm", "sboyer"),
      ("test/R5RS/gambit/nboyer.scm", "nboyer"),
      ("test/R5RS/gambit/matrix.scm", "matrix"),
      ("test/R5RS/gambit/browse.scm", "browse"),
      ("test/R5RS/scp1-compressed/all.scm", "scp"),
      ("test/R5RS/ad/all.scm", "ad"),
      ("test/R5RS/various/SICP-compiler.scm", "SICP"),
      ("test/R5RS/icp/icp_1c_ambeval.scm", "ambeval"),
      ("test/R5RS/icp/icp_1c_multiple-dwelling.scm", "multiple-dwelling"),
      ("test/R5RS/icp/icp_1c_ontleed.scm", "decompose"),
      ("test/R5RS/icp/icp_1c_prime-sum-pair.scm", "prime-sum-pair"),
      ("test/R5RS/icp/icp_7_eceval.scm", "eceval"),
      ("test/R5RS/icp/icp_8_compiler.scm", "compiler"),
      ("test/R5RS/icp/icp_5_regsim.scm", "regsim"),
      ("test/R5RS/icp/icp_3_leval.scm", "leval"),
      ("test/R5RS/icp/icp_2_aeval.scm", "aeval")
    ).toMap
    def all = List(
      // TODO: commented for locally testing only, everything should be uncommented here
      "test/R5RS/gabriel/cpstak.scm"
      //"test/R5RS/WeiChenRompf2019/meta-circ.scm"
      //"test/R5RS/WeiChenRompf2019/earley.sch",
      //"test/R5RS/WeiChenRompf2019/toplas98/graphs.scm",
      //"test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm",
      //"test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm",
      //"test/R5RS/WeiChenRompf2019/toplas98/boyer.scm",
      //"test/R5RS/gambit/peval.scm",
      //"test/R5RS/gambit/scheme.scm",
      //"test/R5RS/gambit/sboyer.scm",
      //"test/R5RS/gambit/nboyer.scm"
//    "test/R5RS/gambit/matrix.scm",
//    "test/R5RS/gambit/browse.scm",
//    "test/R5RS/scp1-compressed/all.scm",
//    "test/R5RS/ad/all.scm",
//    "test/R5RS/various/SICP-compiler.scm",
//    "test/R5RS/icp/icp_1c_ambeval.scm",
//    "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
//    "test/R5RS/icp/icp_1c_ontleed.scm",
//    "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
//    "test/R5RS/icp/icp_7_eceval.scm",
//    "test/R5RS/icp/icp_8_compiler.scm",
//    "test/R5RS/icp/icp_5_regsim.scm",
//    "test/R5RS/icp/icp_3_leval.scm",
//    "test/R5RS/icp/icp_2_aeval.scm",
    )
    val excludedFor2CFA: Set[String] = Set(
      "test/R5RS/WeiChenRompf2019/earley.sch", // Times out with n = 1, 60min timeout
      "test/R5RS/WeiChenRompf2019/meta-circ.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/WeiChenRompf2019/toplas98/boyer.scm", // Takes 23min with n = 1
      "test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/gambit/peval.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/gambit/scheme.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/icp/icp_1c_ambeval.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/icp/icp_1c_multiple-dwelling.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/icp/icp_1c_ontleed.scm", // Times out with n = 1, 60min timeout
      "test/R5RS/icp/icp_1c_prime-sum-pair.scm" // Times out with n = 1, 60min timeout
    )

    def for2CFA = all.filter(b => !(excludedFor2CFA.contains(b)))

    def forDSS = List(
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

    def forDSSFS(k: Int) = 
        List(
            //"test/R5RS/icp/icp_1c_multiple-dwelling.scm",
            //"test/R5RS/icp/icp_1c_ontleed.scm",
            //"test/R5RS/icp/icp_1c_prime-sum-pair.scm"
           //"test/R5RS/icp/icp_7_eceval.scm",
           //"test/R5RS/gambit/peval.scm",
            //"test/R5RS/gambit/scheme.scm",
           //"test/R5RS/gambit/sboyer.scm"    
           "test/R5RS/icp/icp_3_leval.scm"       
        )

    def forDSS(k: Int) = 
        List( 
           "test/R5RS/gabriel/boyer.scm",
           "test/R5RS/gabriel/browse.scm",
           "test/R5RS/gabriel/cpstak.scm",
           "test/R5RS/gabriel/dderiv.scm",
           "test/R5RS/gabriel/deriv.scm",
           "test/R5RS/gabriel/destruc.scm",
           "test/R5RS/gabriel/diviter.scm",
           "test/R5RS/gabriel/divrec.scm",
           "test/R5RS/gabriel/takl.scm",
           "test/R5RS/gabriel/puzzle.scm",           
           "test/R5RS/gabriel/triangl.scm",           
           "test/R5RS/gambit/matrix.scm",           
           "test/R5RS/various/mceval.scm",
           "test/R5RS/various/regex.scm",
           "test/R5RS/various/rsa.scm",
           "test/R5RS/gambit/tak.scm"
        )

//
// MODF BASE RESULTS
//

trait BaseResultsModFSetup extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    override def analysisRuns = 10 // reduced for getting results faster
    override def analysisTime = Timeout.start(Duration(10, MINUTES))
    def k: Int
    def analyses: List[(SchemeExp => Analysis, String)] = List(
      (SchemeAnalyses.kCFAAnalysis(_, k), s"base ModF ($k-CFA)")
    )

object BaseResultsModF0CFA extends BaseResultsModFSetup:
    def k = 0
    def benchmarks = ParallelBenchmarks.all

object BaseResultsModF2CFA extends BaseResultsModFSetup:
    def k = 2
    def benchmarks = ParallelBenchmarks.for2CFA

object BaseResultsModF:
    def loccount(file: String): Int =
        import scala.io.Source
        Source.fromFile(file).getLines().length
    def formatInMinuteSeconds(ms: Double): String =
        val seconds = (ms / 1000).round.toInt
        val minutes = seconds / 60
        val remainingSeconds = seconds - (minutes * 60)
        if minutes > 0 then s"${minutes}m${remainingSeconds}s"
        else s"${seconds}s"
    def formatResult(result: PerformanceResult) = result match
        case Completed(res) => s"${formatInMinuteSeconds(res.mean)} \\pm ${formatInMinuteSeconds(res.stddev)}"
        case TimedOut       => "\\infty"
        case NoData         => "\\infty"
    def main(args: Array[String]): Unit =
        BaseResultsModF0CFA.run()
        BaseResultsModF0CFA.exportCSV("data/modf-base-context-insensitive.csv", BaseResultsModF0CFA.format _, timestamped = false)
        BaseResultsModF0CFA.exportCSV("data/modf-base-context-insensitive.csv-stddev", BaseResultsModF0CFA.formatStddev _, timestamped = false)
        BaseResultsModF2CFA.run()
        BaseResultsModF2CFA.exportCSV("data/modf-base-context-sensitive.csv", BaseResultsModF2CFA.format _, timestamped = false)
        BaseResultsModF2CFA.exportCSV("data/modf-base-context-sensitive.csv-stddev", BaseResultsModF2CFA.formatStddev _, timestamped = false)
        ParallelBenchmarks.all.foreach { (benchmark: String) =>
            val shortName = ParallelBenchmarks.paperName(benchmark)
            val loc = loccount(benchmark)
            val zeroCFA = BaseResultsModF0CFA.results.get(benchmark, "base ModF (0-CFA)").get
            val twoCFA = BaseResultsModF2CFA.results.get(benchmark, "base ModF (2-CFA)").get
            println(s"\\prog{$shortName} & $loc & ${formatResult(zeroCFA)} & ${formatResult(twoCFA)} \\\\ \\hline")
        }

//
// DSS EVALUATION
//

trait ParallelDSSPerformance extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    override def analysisRuns = 10 // reduced for getting results faster
    override def analysisTime = Timeout.start(Duration(30, MINUTES))
    def k: Int
    def outputFile: String
    def cores = List(1, 2, 4, 8)
    def benchmarks: Iterable[Benchmark]
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)
    def analyses: List[(SchemeExp => Analysis, String)] =
        //cores.map(n => (ParallelDSSAnalyses.parallelDSSFS(_, n, k), s"parallel DSS-FS (n = $n, $k-CFA)"))
        List((prg => SchemeAnalyses.modflocalFSAnalysis(prg, 0), "DSS-FS"),
             (prg => SchemeAnalyses.modflocalFSAnalysisNaive(prg, 0), "DSS-FS-NAIVE"))
    def main(args: Array[String]) =
        benchmarks.foreach { file => Reader.loadFile(file) } // sanity check to see if all files exist
        MAFLogger.disable()
        run()
        exportCSV(outputFile, format _, timestamped = false)
        exportCSV(outputFile + "-stddev", formatStddev _, timestamped = false)

object ParallelDSSPerformance0CFA extends ParallelDSSPerformance:
    def k = 0
    def outputFile = "data/dss-fs-iterations.csv"
    def benchmarks = ParallelBenchmarks.forDSS(k)

object ParallelModFPerformance2CFA extends ParallelDSSPerformance:
    def k = 2
    def outputFile = "data/modf-context-sensitive.csv"
    def benchmarks = ParallelBenchmarks.for2CFA


//
// MODCONC EVALUATION
//

object ParallelPerformanceModConc extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    override def analysisRuns = 10 // reduced for getting results faster
    override def analysisTime = Timeout.start(Duration(10, MINUTES))
    def benchmarks: Iterable[String] = List(
      "test/concurrentScheme/threads/crypt.scm"
//    "test/concurrentScheme/threads/actors.scm",
//    "test/concurrentScheme/threads/matmul.scm",
//    "test/concurrentScheme/threads/minimax.scm",
//    "test/concurrentScheme/threads/msort.scm",
//    "test/concurrentScheme/threads/randomness2.scm",
//    "test/concurrentScheme/threads/sieve.scm",
//    "test/concurrentScheme/threads/stm.scm",
//    "test/concurrentScheme/threads/sudoku.scm",
//    "test/concurrentScheme/threads/tsp.scm",
//    "test/concurrentScheme/threads/abp.scm",
//    "test/concurrentScheme/threads/sieve.scm",
//    "test/concurrentScheme/threads/life.scm",
//    "test/concurrentScheme/threads/nbody.scm",
//    "test/concurrentScheme/threads/phild.scm",
//    "test/concurrentScheme/threads/atoms.scm",
//    "test/concurrentScheme/threads/pp.scm",
//    "test/concurrentScheme/threads/pps.scm",
    )

    def cores = List(1, 2) // TODO: 1, 2, 4, 8 for 64-core eval
    def analyses: List[(SchemeExp => Analysis, String)] =
        List((SchemeAnalyses.modConcAnalysis(_, 5), "base ModConc")) ++
            cores.flatMap { n =>
                cores.map { m =>
                    (SchemeAnalyses.parallelModConc(_, n, m, 5), s"parallel (n = $n; m = $m)")
                }
            }
    def main(args: Array[String]) =
        run()
        exportCSV("data/modconc.csv", format, timestamped = false)
