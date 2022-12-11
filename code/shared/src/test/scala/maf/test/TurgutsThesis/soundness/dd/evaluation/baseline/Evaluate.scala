package maf.test.TurgutsThesis.soundness.dd.evaluation.baseline

import maf.test.TurgutsThesis.soundness.dd.evaluation.*

object SaveData:
  def main(args: Array[String]): Unit = {
    org.scalatest.run(new SchemeModFLocalAdaptiveTests1)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests2)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests3)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests4)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests5)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests6)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests7)
    org.scalatest.run(new SchemeModFLocalAdaptiveTests8)

    val baselineDataCollector = DDWithAllTransformationsEval.dataCollector
    baselineDataCollector.writeTo("baselineDataCollector")
  }
  
object ReadAndAnalyzeData:
  def main(args: Array[String]): Unit = {
    val baselineDataCollector = BaselineDataCollector.readObject("baselineDataCollector")

    def aggregate(): Unit =
      val originalSizes = baselineDataCollector.originalSizes.values.flatten
      val reducedSizes = baselineDataCollector.reducedSizes.values.flatten
      val reductionTimes = baselineDataCollector.reductionTimes.values.flatten
      val numberOfProgramsReduced = reductionTimes.size

      println(">>> avgs over all bugs <<<")
      println("number of programs reduced: " + numberOfProgramsReduced)
      println("avg original program size: " + originalSizes.sum / originalSizes.size)
      println("avg reduced program size: " + reducedSizes.sum / reducedSizes.size)
      println("avg reduction time: " + reductionTimes.sum / reductionTimes.size)
      println("") //newline

    def analyzeBug(bugName: String): Unit =
      val bugOriginalSizes = baselineDataCollector.originalSizes(bugName)
      val bugReducedSizes = baselineDataCollector.reducedSizes(bugName)
      val bugReductionTimes = baselineDataCollector.reductionTimes(bugName)
      val numberOfProgramsReduced = bugReductionTimes.size

      println(">>> analyzing bug: " + bugName + " <<<")
      println("number of programs reduced: " + numberOfProgramsReduced)
      println("avg original program size: " + bugOriginalSizes.sum / bugOriginalSizes.size)
      println("avg reduced program size: " + bugReducedSizes.sum / bugReducedSizes.size)
      println("avg reduction time: " + bugReductionTimes.sum / bugReductionTimes.size)
      println("") //newline


    aggregate()
    baselineDataCollector.originalSizes.keySet.foreach(bugName => {
      analyzeBug(bugName)
    })
  }
