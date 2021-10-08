package maf.cli.experiments.incremental

object RunIncrementalEvaluation:
    def main(args: Array[String]): Unit =
        IncrementalSchemeModXPrecision.main(args)
        IncrementalSchemeModXProperties.main(args)
        IncrementalSchemeModXPerformance.main(args) // Run this last to have the other results sooner.
