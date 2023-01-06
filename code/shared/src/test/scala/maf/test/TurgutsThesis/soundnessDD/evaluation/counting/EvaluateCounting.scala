package maf.test.TurgutsThesis.soundnessDD.evaluation.counting

import maf.test.TurgutsThesis.soundnessDD.evaluation.Evaluate
import maf.util.benchmarks.Statistics

object SaveCounting:
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


