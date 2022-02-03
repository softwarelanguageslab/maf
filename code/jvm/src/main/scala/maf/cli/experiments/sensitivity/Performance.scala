package maf.cli.experiments.sensitivity

import maf.language.scheme._
import maf.cli.experiments.performance._
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.modular.AnalysisEntry

object UserGuidedSensitivity1Performance extends PerformanceEvaluation:
    type Analysis = AnalysisEntry[SchemeExp]
    def benchmarks = SchemeBenchmarkPrograms.gabriel

    def analyses: List[(SchemeExp => Analysis, String)] =
      List((PrecisionComparison.baseAnalysis, "base"), (PrecisionComparison.improvedAnalysis, "improved"))

    def main(args: Array[String]) = run()
