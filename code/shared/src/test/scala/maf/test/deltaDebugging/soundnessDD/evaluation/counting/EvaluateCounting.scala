package maf.test.deltaDebugging.soundnessDD.evaluation.counting

import maf.test.deltaDebugging.soundnessDD.evaluation.Evaluate
import maf.util.benchmarks.Statistics

object SaveCounting:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
      ),
      "countingDataCollector",
      CountingDD.dataCollector
    )
  }
  
object ReadAndAnalyzeCounting:  
  def main(args: Array[String]): Unit = {
    Evaluate.readAndAnalyzeData(
      "countingDataCollector"
    )
  }


