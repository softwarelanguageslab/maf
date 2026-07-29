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
import maf.modular.worklist.RandomWorklistAlgorithm
import maf.modular.worklist.CallDepthFirstWorklistAlgorithm
import maf.cli.experiments.parallel.DSSFSBenchmarks

object ParallelDSSAnalyses:

    class ParallelDSSAnalysis(prg: SchemeExp, n: Int, kcfa: Int) 
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

    class ParallelDSSFSAnalysis(prg: SchemeExp, n: Int, kcfa: Int) 
        extends SchemeModFLocalFS(prg, gc=true)
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
        new ParallelDSSAnalysis(prg, n, kcfa)// with CallDepthFirstWorklistAlgorithm[SchemeExp]

    def parallelDSSFS(prg: SchemeExp, n: Int, kcfa: Int) = 
        new ParallelDSSFSAnalysis(prg, n, kcfa)// with CallDepthFirstWorklistAlgorithm[SchemeExp]

object ParallelModFAnalyses:

    class ParallelModFAnalysis(prg: SchemeExp, n: Int, kcfa: Int)
        extends ModAnalysis(prg) with SchemeModFSemanticsM
                                 with StandardSchemeModFComponents
                                 with BigStepModFSemantics
                                 with ParallelWorklistAlgorithm[SchemeExp]
                                 //with CallDepthFirstWorklistAlgorithm[SchemeExp]
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

    def SCAMbenchmarks = 
        List(
            "test/R5RS/icp/icp_1c_multiple-dwelling.scm",        //~18min.
            "test/R5RS/icp/icp_1c_ontleed.scm",
            "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
            "test/R5RS/icp/icp_7_eceval.scm",
            "test/R5RS/gambit/peval.scm",
            "test/R5RS/gambit/scheme.scm",
            "test/R5RS/gambit/sboyer.scm",
            "test/R5RS/WeiChenRompf2019/toplas98/graphs.scm",
            "test/R5RS/gambit/matrix.scm",
            "test/R5RS/gambit/nboyer.scm",
            "test/R5RS/gambit/browse.scm",
            "test/R5RS/icp/icp_8_compiler.scm",
            "test/R5RS/icp/icp_3_leval.scm"
        )

    def DSSbenchmarks = 
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
           "test/R5RS/gambit/tak.scm",
           "test/R5RS/various/grid.scm"
        )

//
// MODF BASE RESULTS
//

trait BaseResultsModFSetup extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    override def analysisRuns = 5 // reduced for getting results faster
    override def analysisTime = Timeout.start(Duration(20, MINUTES))
    def k: Int
    def analyses: List[(SchemeExp => Analysis, String)] = List(
      (SchemeAnalyses.kCFAAnalysis(_, k), s"base ModF ($k-CFA)")
    )

object BaseResultsModF0CFA extends BaseResultsModFSetup:
    def k = 0
    def benchmarks = ParallelBenchmarks.all

//
// DSS EVALUATION
//

trait ParallelDSSPerformance extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    override def analysisRuns = 30 // reduced for getting results faster
    override def analysisTime = Timeout.start(Duration(120, MINUTES))
    def outputFile: String
    def benchmarks: Iterable[Benchmark]
    override def parseProgram(txt: String): SchemeExp =
        val parsed = SchemeParser.parse(txt)
        val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
        val transf = SchemeMutableVarBoxer.transform(prelud)
        SchemeParser.undefine(transf)
    def main(args: Array[String]) =
        benchmarks.foreach { file => Reader.loadFile(file) } // sanity check to see if all files exist
        MAFLogger.disable()
        run()
        exportCSV(outputFile, format _, timestamped = false)
        exportCSV(outputFile + "-stddev", formatStddev _, timestamped = false)

trait DSSFSBenchmarks(k: Int) extends ParallelDSSPerformance:
    def outputFile = s"data/dss-benchmarks-testing-dssfs-k=$k.csv"
    override def analysisTime = Timeout.start(Duration(60, MINUTES))
    def benchmarks = ParallelBenchmarks.SCAMbenchmarks
    def cores = List(1,2,4,8,16,32,64)
    def analyses =
        (SchemeAnalyses.modflocalFSAnalysis(_, k, true), s"DSS-FS (k=$k)")
        ::
        cores.map { n => 
            (ParallelDSSAnalyses.parallelDSSFS(_, n, k), s"PDSS-FS (n=$n/k=$k)")
        }

object ContextInsensitiveDSSFSAnalyses extends DSSFSBenchmarks(0):
    override def benchmarks = 
        super.benchmarks.filterNot(Set(
            "test/R5RS/icp/icp_7_eceval.scm",
            "test/R5RS/gambit/scheme.scm"
        ))

object ContextSensitiveDSSFSAnalyses extends DSSFSBenchmarks(2):
    override def benchmarks = List("test/R5RS/gambit/matrix.scm")
       /*
        super.benchmarks.filterNot(Set(
            "test/R5RS/icp/icp_7_eceval.scm",
            "test/R5RS/gambit/scheme.scm",
            "test/R5RS/gambit/peval.scm",
            "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
            "test/R5RS/icp/icp_1c_ontleed.scm",
            "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
        ))
        */

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
