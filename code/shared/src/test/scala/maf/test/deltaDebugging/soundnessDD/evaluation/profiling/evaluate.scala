package maf.test.deltaDebugging.soundnessDD.evaluation.profiling

import maf.util.benchmarks.Statistics

object SaveData:
  def main(args: Array[String]): Unit = {
    org.scalatest.run(new EvalProfilingTestSuiteA)

    val withProfilingDataCollector = WithProfilingDD.dataCollector
    //val withoutProfilingDataCollector = WithoutProfilingDD.dataCollector

    withProfilingDataCollector.writeTo("withProfilingDataCollector")
    //withoutProfilingDataCollector.writeTo("withoutProfilingDataCollector")
  }

object ReadAndAnalyzeData:
  def main(args: Array[String]): Unit = {
    val withProfilingDataCollector = ProfilingDataCollector.readObject("withProfilingDataCollector")
    val withoutProfilingDataCollector = ProfilingDataCollector.readObject("withoutProfilingDataCollector")

    def evaluateDataCollector(collector: ProfilingDataCollector): Unit = {
      val data = collector.data
      val programSizes = data.map(_.programSize)
      val reducedSizes = data.map(_.reducedSize)
      val reductionTimes = data.map(_.reductionTime)
      val oracleCounts = data.map(_.oracleCount)
      val oracleHits = data.map(_.OracleHits)
      val oracleTimes = data.map(_.oracleTimes)
      val analysisSteps = data.map(_.analysisSteps)
      val candidateFunctionCounts = data.map(_.candidateFunctionCount)
      val totalFunctionCounts = data.map(_.totalFunctionCount)

      val numberOfPrograms = programSizes.size
      val averageReductionTime = reductionTimes.sum / reductionTimes.size
      val averageOracleCount = oracleCounts.sum / oracleCounts.size
      val averageOracleTime = oracleTimes.flatten.map(tpl => tpl._1).sum / oracleTimes.flatten.size
      val averageOriginalProgramSize = programSizes.sum / programSizes.size
      val averageReducedProgramSize = reducedSizes.sum / reducedSizes.size
      val averageNumberOfFunctionsRemoved: Double = oracleHits.sum.toDouble / oracleHits.size
      val averageNumberOfFunctions: Double = totalFunctionCounts.sum.toDouble / totalFunctionCounts.size
      val averageNumberOfCandidateFunctions: Double = candidateFunctionCounts.sum.toDouble / candidateFunctionCounts.size

      def findTotalAnalysisStepsAfterNReductions(lst: Iterable[(Int, Int)], n: Int): Int =
        lst.filter(tpl => tpl._2 == n).map(_._1).sum

      def findAverageAnalysisStepsAfterNReductions(lst: Iterable[(Int, Int)], n: Int): Int =
        (lst.filter(tpl => tpl._2 == n).map(_._1).sum) / (lst.filter(tpl => tpl._2 == n).map(_._1).size)

      def findTotals(lst: Iterable[(Int, Int)]): List[Int] =
        0.to(10).map(nmbr => findTotalAnalysisStepsAfterNReductions(lst, nmbr)).toList

      def findAverages(lst: Iterable[(Int, Int)]): List[Int] =
        0.to(10).map(nmbr => findAverageAnalysisStepsAfterNReductions(lst, nmbr)).toList

      val totals = findTotals(analysisSteps.flatten)

      val averages = findAverages(analysisSteps.flatten)

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

      println("total number of analysis steps: " + analysisSteps.flatten.map(tpl => tpl._1).sum)

      averages.zipWithIndex.foreach(tpl => {
        val avg = tpl._1
        val idx = tpl._2
        println("avg number of steps for analysis after " + idx + " reductions: " + avg)
      })
    }

    val medianProgramSize = Statistics.median(withProfilingDataCollector.data.map(r => r.programSize))
    val largeWithProfiling = withProfilingDataCollector.filter(medianProgramSize.toInt)
    val largeWithoutProfiling = withoutProfilingDataCollector.filter(medianProgramSize.toInt)

    println(">>>>> results WITHOUT profiling <<<<< ")
    evaluateDataCollector(withoutProfilingDataCollector)
    println("")
    println("####################################")
    println("")
    println(">>>>> results WITH profiling <<<<< ")
    evaluateDataCollector(withProfilingDataCollector)

    println(">>>>> results LARGE WITHOUT profiling <<<<< ")
    evaluateDataCollector(largeWithoutProfiling)
    println("")
    println("####################################")
    println("")
    println(">>>>> results LARGE WITH profiling <<<<< ")
    evaluateDataCollector(largeWithProfiling)
  }
