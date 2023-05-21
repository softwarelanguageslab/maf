package maf.test.deltaDebugging.soundnessDD.variants.SDCE

import maf.test.deltaDebugging.soundnessDD.variants.Evaluate
import maf.util.benchmarks.Statistics

object SaveTransforming:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "transformingDataCollector",
      SDCE_DD.dataCollector
    )
  }
