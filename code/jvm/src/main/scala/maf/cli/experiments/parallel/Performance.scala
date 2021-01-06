package maf.cli.experiments.parallel

import maf.language.scheme._
import maf.cli.experiments._
import maf.cli.experiments.performance._

object ParallelModFPerformance1 extends PerformanceEvaluation {
  def benchmarks = Set(
    "test/R5RS/gambit/scheme.scm",
    "test/R5RS/gambit/peval.scm",
    "test/R5RS/gambit/sboyer.scm",
    "test/R5RS/icp/icp_1c_ontleed.scm",
    "test/R5RS/icp/icp_1c_multiple-dwelling.scm",
    "test/R5RS/icp/icp_1c_prime-sum-pair.scm",
    "test/R5RS/icp/icp_7_eceval.scm"
  )
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (SchemeAnalyses.kCFAAnalysis(_, 0), "base ModF"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 1, 0), "parallel (n = 1)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 2, 0), "parallel (n = 2)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 4, 0), "parallel (n = 4)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 8, 0), "parallel (n = 8)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 16, 0), "parallel (n = 16)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 32, 0), "parallel (n = 32)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 64, 0), "parallel (n = 64)")
  )
  def main(args: Array[String]) = run()
}

object ParallelModFPerformance2 extends PerformanceEvaluation {
  def benchmarks = Set(
    "test/R5RS/gambit/graphs.scm",
    "test/R5RS/gambit/matrix.scm",
    "test/R5RS/gambit/nboyer.scm",
    "test/R5RS/gambit/sboyer.scm",
    "test/R5RS/gambit/browse.scm",
    "test/R5RS/icp/icp_8_compiler.scm",
    "test/R5RS/icp/icp_3_leval.scm"
  )
  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (SchemeAnalyses.kCFAAnalysis(_, 2), "base ModF (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 1, 2), "parallel (n = 1) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 2, 2), "parallel (n = 2) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 4, 2), "parallel (n = 4) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 8, 2), "parallel (n = 8) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 16, 2), "parallel (n = 16) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 32, 2), "parallel (n = 32) (2-CFA)"),
    (SchemeAnalyses.parallelKCFAAnalysis(_, 64, 2), "parallel (n = 64) (2-CFA)")
  )
  def main(args: Array[String]) = run()
}

object ParallelModConcPerformance extends PerformanceEvaluation {
  def benchmarks = Set(
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
