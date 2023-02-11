package maf.test.deltaDebugging.soundnessDD.evaluation.transforming

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate
import maf.util.benchmarks.Statistics

object SaveTransforming:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
      ),
      "transformingDataCollector",
      TransformingDD.dataCollector
    )
  }

object ReadAndAnalyzeTransforming:
  def main(args: Array[String]): Unit = {
    Evaluate.readAndAnalyzeData(
      "transformingDataCollector"
    )
  }

