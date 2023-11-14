package maf.test.deltaDebugging.soundnessDD.variants.baseline

import maf.test.DDBenchmarks
import maf.test.deltaDebugging.soundnessDD.variants.Evaluate
import maf.util.benchmarks.Statistics

object SaveBaseline:
  def save(): Unit = {
    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "baselineDataCollector",
      BaselineDD.dataCollector
    )
  }


object Wrapper extends DDBenchmarks:
  override protected def onBenchmark(b: Wrapper.Benchmark): Unit = None
  def main(args: Array[Benchmark]): Unit = {
    benchmarks.foreach(println)
  }
