package maf.test.deltaDebugging.soundnessDD.evaluation.profiling

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate

object SaveProfiling:
  def save(): Unit =
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        //new SchemeModFLocalAdaptiveTests4,
        //new SchemeModFLocalAdaptiveTests5
      ),
      "profilingDataCollector",
      ProfilingDD.dataCollector
    )
