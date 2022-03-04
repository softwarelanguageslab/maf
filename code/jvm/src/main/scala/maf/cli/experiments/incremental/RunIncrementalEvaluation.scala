package maf.cli.experiments.incremental

object RunIncrementalEvaluation:
    def main(args: Array[String]): Unit =
        try IncrementalSchemeModXPrecision.main(args)
        catch
            case t: Throwable =>
              System.err.nn.println(s"Measuring precision failed: ${t.getMessage}.")
              /*
        try IncrementalSchemeModXProperties.main(args)
        catch
            case t: Throwable =>
              System.err.nn.println(s"Obtaining properties failed: ${t.getMessage}.")
        try IncrementalSchemeModXPerformance.main(args) // Run this last to have the other results sooner.
        catch
            case t: Throwable =>
              System.err.nn.println(s"Measuring performance failed: ${t.getMessage}.")
              */
