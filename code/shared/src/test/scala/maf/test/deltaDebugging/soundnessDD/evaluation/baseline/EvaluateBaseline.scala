package maf.test.deltaDebugging.soundnessDD.evaluation.baseline

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate
import maf.util.benchmarks.Statistics

object SaveBaseline:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        //new SchemeModFLocalAdaptiveTests3,
        //new SchemeModFLocalAdaptiveTests4,
        //new SchemeModFLocalAdaptiveTests5
      ),
      "baselineDataCollector",
      BaselineDD.dataCollector
    )
  }

object ReadAndAnalyzeBaseline:
  def main(args: Array[String]): Unit = {
    Evaluate.readAndAnalyzeData("baselineDataCollector")
  }

