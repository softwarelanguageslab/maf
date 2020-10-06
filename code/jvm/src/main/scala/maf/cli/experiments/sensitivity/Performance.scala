package maf.cli.experiments.sensitivity

import maf.language.scheme._
import maf.language.CScheme._
import maf.modular.ModAnalysis
import maf.util._
import maf.util.benchmarks._
import maf.cli.experiments._
import maf.cli.experiments.performance._

object UserGuidedSensitivity1Performance extends PerformanceEvaluation {
  def benchmarks = Set("test/R5RS/mceval.scm")

  def analyses: List[(SchemeExp => Analysis, String)] = List(
    (PrecisionComparison.baseAnalysis, "base"),
    (PrecisionComparison.improvedAnalysis, "improved"))

  def main(args: Array[String]) = run()
}
