package maf.cli.experiments.aam

import maf.language.scheme.*
import maf.cli.experiments.performance.PerformanceEvaluation
import maf.modular.AnalysisEntry

/* Reproduction of State Exploration Choices in a Small-Step Abstract Interpreter, S. Lyde & M. Might */
object ExplorationStrategies extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]

    val analyses: List[(SchemeExp => AnalysisEntry[SchemeExp], String)] = List(
      (AAMAnalyses.aamBase, "base")
    )

    def benchmarks: Set[String] = Set(
      "test/R5RS/various/eta.scm",
      "test/R5RS/various/map.scm",
      "test/R5RS/various/sat.scm",
      "test/R5RS/various/regex.scm",
      "test/R5RS/various/scm2java.scm",
      // We were unable to find the interp program
      "test/R5RS/various/scm2c.scm"
    )

    def main(args: Array[String]): Unit =
        run()
