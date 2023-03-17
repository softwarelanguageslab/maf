package maf.test.deltaDebugging.soundnessDD.variants.smartReplacement

import maf.test.DDBenchmarks
import maf.test.deltaDebugging.soundnessDD.variants.Evaluate
import maf.util.benchmarks.Statistics

object SaveSmartReplacement:
  def save(): Unit = {
    Evaluate.save(
      List(
        //new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        //new SchemeModFLocalAdaptiveTests4,
        //new SchemeModFLocalAdaptiveTests5
      ),
      "smartReplacementDataCollector",
      SmartReplacementDD.dataCollector
    )
  }

/*
object Wrapper extends DDBenchmarks:
  override protected def onBenchmark(b: Wrapper.Benchmark): Unit = None
  def main(args: Array[Benchmark]): Unit = {
    benchmarks.foreach(println)
  }
*/
