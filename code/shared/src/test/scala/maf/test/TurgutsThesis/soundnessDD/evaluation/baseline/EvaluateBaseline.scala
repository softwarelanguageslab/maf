package maf.test.TurgutsThesis.soundnessDD.evaluation.baseline

import maf.test.TurgutsThesis.soundnessDD.evaluation.Evaluate
import maf.util.benchmarks.Statistics

object SaveBaseline:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5,
        new SchemeModFLocalAdaptiveTests6,
        new SchemeModFLocalAdaptiveTests7,
        new SchemeModFLocalAdaptiveTests8,
      ),
      "baselineDataCollector",
      BaselineDD.dataCollector
    )
  }

object ReadAndAnalyzeBaseline:
  def main(args: Array[String]): Unit = {
    Evaluate.readAndAnalyzeData("baselineDataCollector")
  }

