package maf.test.deltaDebugging.soundnessDD.variants.killLambda

import maf.test.deltaDebugging.soundnessDD.variants.Evaluate

object SaveDeadCode:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "deadCodeDataCollector",
      KillLambdaDD.dataCollector
    )
  }
