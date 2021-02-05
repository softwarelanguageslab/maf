package maf.cli.experiments.parallel

import maf.language.scheme._
import maf.cli.experiments._
import maf.cli.experiments.performance._
import maf.util.benchmarks._
import scala.concurrent.duration._

trait ParallelModFPerformance extends PerformanceEvaluation {
  override def analysisTime = Timeout.start(Duration(10, MINUTES)) // TODO: increase for actual evaluation
  def k: Int
  def cores = List(1, 2, 4, 8)
  def benchmarks: Set[Benchmark]
  def analyses: List[(SchemeExp => Analysis, String)] = {
    (SchemeAnalyses.kCFAAnalysis(_, k), s"base ModF ($k-CFA)") ::
    cores.map(n => (SchemeAnalyses.parallelKCFAAnalysis(_, n, k), s"parallel (n = $n, $k-CFA)"))
  }
  def main(args: Array[String]) = run()
}

object ParallelModFPerformance0CFA extends ParallelModFPerformance {
  def k = 0
  def benchmarks = Set(
    "test/R5RS/WeiChenRompf2019/meta-circ.scm",
    "test/R5RS/WeiChenRompf2019/earley.sch",
    "test/R5RS/WeiChenRompf2019/toplas98/dynamic.scm",
    "test/R5RS/WeiChenRompf2019/toplas98/nbody-processed.scm",
    "test/R5RS/WeiChenRompf2019/toplas98/boyer.scm",
    "test/R5RS/gambit/peval.scm",
    "test/R5RS/gambit/scheme.scm",
    "test/R5RS/gambit/sboyer.scm",
    "test/R5RS/gambit/nboyer.scm",
    "test/R5RS/scp1-compressed/all.scm",
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
}

object ParallelModFPerformance1CFA extends ParallelModFPerformance {
  def k = 1
  def benchmarks = ParallelModFPerformance0CFA.benchmarks
}

object ParallelModFPerformance2CFA extends ParallelModFPerformance {
  def k = 2
  def benchmarks = ParallelModFPerformance0CFA.benchmarks
}

object ParallelModConcPerformance extends PerformanceEvaluation {
  def benchmarks: Set[String] = Set(
    "test/concurrentScheme/threads/crypt.scm",
    "test/concurrentScheme/threads/actors.scm",
    "test/concurrentScheme/threads/matmul.scm",
    "test/concurrentScheme/threads/minimax.scm",
    "test/concurrentScheme/threads/msort.scm",
    "test/concurrentScheme/threads/randomness2.scm",
    "test/concurrentScheme/threads/sieve.scm",
    "test/concurrentScheme/threads/stm.scm",
    "test/concurrentScheme/threads/sudoku.scm",
    "test/concurrentScheme/threads/tsp.scm"
  )
  def analyses: List[(SchemeExp => Analysis, String)] =
    List((SchemeAnalyses.modConcAnalysis(_, 5), "base ModConc")) ++
      List(1, 2, 4, 8).flatMap { n =>
        List(1, 2, 4, 8).map { m =>
          (SchemeAnalyses.parallelModConc(_, n, m, 5), s"parallel (n = $n; m = $m)")
        }
      }
  def main(args: Array[String]) = run()
}
