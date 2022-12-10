package maf.test.TurgutsThesis.soundness.evaluation

import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.{DDWithProfilingEval, DDWithoutProfilingEval, ProfilingDataCollector}

object EvaluateCollectedData:

  def main(args: Array[String]): Unit = {
    evaluateData()
  }

  def evaluateData(): Unit =
    def evaluateDataCollector(dataCollector: ProfilingDataCollector): Unit =
      val numberOfPrograms = dataCollector.reducedSizes.values.size
      val averageReductionTime = dataCollector.reductionTimes.values.sum / dataCollector.reductionTimes.values.size
      val averageOracleCount = dataCollector.oracleCounts.values.sum / dataCollector.oracleCounts.values.size
      val averageOracleTime = dataCollector.oracleTimes.values.flatMap(lst => lst.map(_._1)).sum / dataCollector.oracleTimes.values.flatMap(lst => lst.map(_._1)).size
      val averageOriginalProgramSize = dataCollector.programSizes.values.sum / dataCollector.programSizes.values.size
      val averageReducedProgramSize = dataCollector.reducedSizes.values.sum / dataCollector.reducedSizes.values.size
      val averageNumberOfFunctionsRemoved: Double = dataCollector.oracleHits.values.sum.toDouble / dataCollector.oracleHits.values.size
      val averageNumberOfFunctions: Double = dataCollector.functionCount.values.sum.toDouble / dataCollector.functionCount.values.size
      val averageNumberOfCandidateFunctions: Double = dataCollector.candidateFunctionCount.values.sum.toDouble / dataCollector.candidateFunctionCount.values.size


      def findTotalAnalysisStepsAfterNReductions(lst: Iterable[(Int, Int)], n: Int): Int =
        lst.filter(tpl => tpl._2 == n).map(_._1).sum

      def findAverageAnalysisStepsAfterNReductions(lst: Iterable[(Int, Int)], n: Int): Int =
        (lst.filter(tpl => tpl._2 == n).map(_._1).sum) / (lst.filter(tpl => tpl._2 == n).map(_._1).size)

      def findTotals(lst: Iterable[(Int, Int)]): List[Int] =
        0.to(10).map(nmbr => findTotalAnalysisStepsAfterNReductions(lst, nmbr)).toList


      def findAverages(lst: Iterable[(Int, Int)]): List[Int] =
        0.to(10).map(nmbr => findAverageAnalysisStepsAfterNReductions(lst, nmbr)).toList

      //analysis steps:
      val totals = findTotals(dataCollector.analysisSteps.values.flatten)

      val averages = findAverages(dataCollector.analysisSteps.values.flatten)

      println("number of programs: " + numberOfPrograms)
      println("average reduction time: " + averageReductionTime)
      println("average oracle count: " + averageOracleCount)
      println("average oracle time: " + averageOracleTime)
      println("average original program size: " + averageOriginalProgramSize)
      println("average reduced program size: " + averageReducedProgramSize)
      println("average number of functions removed: " + averageNumberOfFunctionsRemoved)
      println("average number of functions: " + averageNumberOfFunctions)
      println("average number of candidate functions: " + averageNumberOfCandidateFunctions)

      println("#####")

      println("total number of analysis steps: " + dataCollector.analysisSteps.values.flatten.map(tpl => tpl._1).sum)

      totals.zipWithIndex.foreach(tpl => {
        val total = tpl._1
        val idx = tpl._2
        println("analysis steps after " + idx + " reductions: " + total)
      })

      averages.zipWithIndex.foreach(tpl => {
        val avg = tpl._1
        val idx = tpl._2
        println("avg number of steps for analysis after " + idx + " reductions: " + avg)
      })

    val withProfilingDataCollector = ProfilingDataCollector.readObject("withProfilingDataCollector")
    val withoutProfilingDataCollector = ProfilingDataCollector.readObject("withoutProfilingDataCollector")

    println(">>>>> results WITHOUT profiling <<<<< ")
    evaluateDataCollector(withoutProfilingDataCollector)
    println("")
    println("")
    println("####################################")
    println("")
    println("")
    println(">>>>> results WITH profiling <<<<< ")
    evaluateDataCollector(withProfilingDataCollector)
