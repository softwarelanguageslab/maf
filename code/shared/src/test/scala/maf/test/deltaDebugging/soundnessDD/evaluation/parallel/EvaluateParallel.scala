package maf.test.deltaDebugging.soundnessDD.evaluation.parallel

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate

object SaveParallel:
  def save(): Unit =
    Evaluate.save(
      List(
      new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "parallelDataCollector",
      ParallelDD.dataCollector
    )

object ReadAndAnalyzeParallel:
  def main(args: Array[String]): Unit = {
    Evaluate.readAndAnalyzeData(
      "parallelDataCollector"
    )
  }
