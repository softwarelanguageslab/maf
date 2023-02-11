package maf.test.deltaDebugging.soundnessDD.evaluation.parallel

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate

object SaveParallel:
  def save(): Unit =
    Evaluate.save(
      List(
      new SchemeModFLocalAdaptiveTests1,
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
