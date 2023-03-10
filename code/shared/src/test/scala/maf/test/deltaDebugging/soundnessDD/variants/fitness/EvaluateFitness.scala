package maf.test.deltaDebugging.soundnessDD.variants.fitness

import maf.deltaDebugging.gtr.transformations.TransformationManager
import maf.test.DDBenchmarks
import maf.test.deltaDebugging.soundnessDD.variants.Evaluate
import maf.util.benchmarks.Statistics

object SaveFitness:
  def save(): Unit = {
    TransformationManager.allTransformations.foreach(t => {
      println("pre " + t.name + ": " + (t.getHits, t.getSuggestedCount))
    })

    Evaluate.save(
      List(
        new SchemeModFLocalAdaptiveTests1,
        new SchemeModFLocalAdaptiveTests2,
        new SchemeModFLocalAdaptiveTests3,
        new SchemeModFLocalAdaptiveTests4,
        new SchemeModFLocalAdaptiveTests5
      ),
      "randomDataCollector",
      FitnessDD.dataCollector
    )

    TransformationManager.allTransformations.foreach(t => {
      println("post " + t.name + ": " + (t.getHits, t.getSuggestedCount))
    })
  }
