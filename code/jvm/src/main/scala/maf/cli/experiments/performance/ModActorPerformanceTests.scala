package maf.cli.experiments.performance

import maf.language.scheme.SchemeExp
import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.cli.experiments.SchemeAnalyses
import maf.modular.scheme.modactor.SchemeModActorSemantics

object ModActorPerformanceTests extends PerformanceEvaluation:
    override type Analysis = SchemeModActorSemantics
    def benchmarks: Iterable[Benchmark] = SchemeBenchmarkPrograms.actors
    def analyses: List[(SchemeExp => Analysis, String)] =
        List(
          (SchemeAnalyses.modActorAnalysis, "modActor")
        )

    def main(args: Array[String]): Unit =
        run("benchOutput/performance/modactor-results.csv")
