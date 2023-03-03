package maf.test.deltaDebugging.soundnessDD.evaluation.deadCode

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate

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
      DeadCodeDD.dataCollector
    )
  }
