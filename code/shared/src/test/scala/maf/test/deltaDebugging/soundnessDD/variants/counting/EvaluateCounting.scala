package maf.test.deltaDebugging.soundnessDD.variants.counting

import maf.test.deltaDebugging.soundnessDD.variants.Evaluate
import maf.util.benchmarks.Statistics

object SaveCounting:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "countingDataCollector",
      CountingDD.dataCollector
    )
  }


