package maf.test.deltaDebugging.soundnessDD.evaluation

import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.evaluation.baseline.SaveBaseline
import maf.test.deltaDebugging.soundnessDD.evaluation.counting.SaveCounting
import maf.test.deltaDebugging.soundnessDD.evaluation.parallel.SaveParallel
import maf.test.deltaDebugging.soundnessDD.evaluation.transforming.{SaveTransforming, SchemeModFLocalAdaptiveTests1, TransformingDD}
import maf.util.benchmarks.Statistics

object Evaluate:
  def main(args: Array[String]): Unit = {
    //SaveParallel.save()
    SaveBaseline.save()
    SaveCounting.save()
    SaveTransforming.save()
  }

  def save(tests: List[SoundnessDDTester], dataCollectorString: String, dataCollector: DataCollector): Unit = {
    tests.foreach(t => org.scalatest.run(t))
    dataCollector.writeTo(dataCollectorString)
  }

  def readAndAnalyzeData(dataCollectorString: String): Unit = {
    val dataCollector: DataCollector = DataCollector.readObject(dataCollectorString)

    def meanAndStdDev(lst: List[Double]): (Double, Double) =
      (Statistics.mean(lst).floor, Statistics.stddev(lst).floor)

    def computeAverages(data: List[ReductionData]): List[(Double)] =
      List(
        Statistics.mean(data.map(r => r.origSize)).floor,
        Statistics.mean(data.map(r => r.reducedSize)).floor,
        Statistics.mean(data.map(r => r.reductionTime)).floor,
        Statistics.mean(data.map(r => r.interpreterTime)).floor,
        Statistics.mean(data.map(r => r.analysisTime)).floor,
        Statistics.mean(data.map(r => r.interpreterTimes.size)).floor
      )

    def analyseData(bugName: Option[String], data: List[ReductionData]): Unit =
      val averages = computeAverages(data)
      val avgOriginalSize = averages(0)
      val avgReducedSize = averages(1)
      val avgReductionTime = averages(2)
      val avgInterpreterTime = averages(3)
      val avgAnalysisTime = averages(4)
      val avgNumberOfOracleInvocations = averages(5)

      if bugName.isEmpty then
        println(">>>>> analysis of all data <<<<<")
      else println(">>>>> analysis bug: " + bugName + " <<<<<")
      println("number of programs: " + data.length)
      println("avg original size: " + avgOriginalSize)
      println("avg reduced size:  " + avgReducedSize)
      println("avg reduction time: " + avgReductionTime)
      println("avg interpreter time: " + avgInterpreterTime)
      println("avg analysis time: " + avgAnalysisTime)
      println("avg # of oracle invocations: " + avgNumberOfOracleInvocations)
      println("##################################################")

      val medianOrigSize = Statistics.Q3(data.map(r => r.origSize))
      val dataForLargePrograms = data.filter(r => r.origSize >= medianOrigSize)
      val largeAverages = computeAverages(dataForLargePrograms)
      val largeAvgOriginalSize = largeAverages(0)
      val largeAvgReducedSize = largeAverages(1)
      val largeAvgReductionTime = largeAverages(2)
      val largeAvgInterpreterTime = largeAverages(3)
      val largeAvgAnalysisTime = largeAverages(4)
      val largeAvgNumberOfOracleInvocations = largeAverages(5)

      println("FOR LARGE PROGRAMS: ")
      println("large: avg original size: " + largeAvgOriginalSize)
      println("large: avg reduced size:  " + largeAvgReducedSize)
      println("large: avg reduction time: " + largeAvgReductionTime)
      println("large: avg interpreter time: " + largeAvgInterpreterTime)
      println("large: avg analysis time: " + largeAvgAnalysisTime)
      println("avg # of oracle invocations: " + largeAvgNumberOfOracleInvocations)

    def analyseIndividualBugs(): Unit =
      val grouped = dataCollector.reductionData.groupBy(r => r.bugName)
      grouped.keySet.foreach(bugName => {
        val data = grouped(bugName)
        val medianSize = Statistics.median(data.map(r => r.origSize.toDouble))
        analyseData(Some(bugName), data)
      })

    analyseData(None, dataCollector.reductionData)

    println(dataCollector.reductionData.map(r => r.reducedSize))
  }

