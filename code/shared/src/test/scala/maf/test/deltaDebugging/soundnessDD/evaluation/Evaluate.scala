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
    tests.foreach(t => t.benchmarks.foreach(t.benchmarks(_)))
    dataCollector.writeTo(dataCollectorString)
  }

  def readAndAnalyzeData(dataCollectorString: String): Unit = {
    val dataCollector: DataCollector = DataCollector.readObject(dataCollectorString)

    def computeMedians(data: List[ReductionData]): List[Double] =
      List(
        Statistics.mean(data.map(r => r.origSize)).floor,
        Statistics.mean(data.map(r => r.reducedSize)).floor,
        Statistics.mean(data.map(r => r.reductionTime / Math.max(r.origCost._2 + r.origCost._1, 1))).floor,
        Statistics.mean(data.map(r => r.reductionTime)),
        Statistics.mean(data.map(r => r.interpreterTime)),
        Statistics.mean(data.map(r => r.interpreterTimes.size)).floor,
        Statistics.mean(data.map(r => r.reductionPercentage)),
        Statistics.mean(data.map(r => r.interpreterPercentage)),
        Statistics.mean(data.map(r => r.analysisPercentage)),
      )

    def analyseData(bugName: Option[String], data: List[ReductionData]): Unit =
      val averages = computeMedians(data)
      val avgOriginalSize = averages(0)
      val avgReducedSize = averages(1)
      val avgRelRedTime = averages(2)
      val avgRedTime = averages(3)
      val avgInterpreterTime = averages(4)
      val avgNumberOfOracleInvocations = averages(5)
      val avgReductionPercentage = averages(6)
      val avgInterpreterTimePercentage = averages(7)
      val avgAnalysisTimePercentage = averages(8)

      if bugName.isEmpty then
        println(">>>>> analysis of all data <<<<<")
      else println(">>>>> analysis bug: " + bugName + " <<<<<")

      println("number of programs reduced: " + data.length)
      println("avg original size: " + avgOriginalSize)
      println("avg reduced size:  " + avgReducedSize)
      println("avg reduction time: "+ avgRedTime)
      println("avg interpreter time: " + avgInterpreterTime)
      println("avg relative time cost: " + avgRelRedTime)
      println("avg # of oracle invocations: " + avgNumberOfOracleInvocations)
      println("##################################################")

      println("avg reduction percentage: " + avgReductionPercentage)
      println("avg interpreter time percentage: " + avgInterpreterTimePercentage)
      println("avg analysis time percentage: " + avgAnalysisTimePercentage)

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

