package maf.test.deltaDebugging.soundnessDD.evaluation

import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.evaluation.baseline.SaveBaseline
import maf.test.deltaDebugging.soundnessDD.evaluation.counting.{CountingDD, SaveCounting}
import maf.test.deltaDebugging.soundnessDD.evaluation.parallel.SaveParallel
import maf.test.deltaDebugging.soundnessDD.evaluation.profiling.SaveProfiling
import maf.test.deltaDebugging.soundnessDD.evaluation.transforming.{SaveTransforming, SchemeModFLocalAdaptiveTests1, TransformingDD}
import maf.util.benchmarks.Statistics

object Evaluate:
  def main(args: Array[String]): Unit = {
    //SaveBaseline.save()
    //SaveCounting.save()
    //SaveTransforming.save()
    //SaveParallel.save()
    SaveProfiling.save()
  }

  def save(tests: List[SoundnessDDTester], dataCollectorString: String, dataCollector: DataCollector): Unit = {
    tests.foreach(t => t.benchmarks.foreach(t.benchmarks(_)))
    dataCollector.writeTo(dataCollectorString)
  }

  /*
  def RQ1(baselineData: List[ReductionData], transformingData: List[ReductionData]): Unit =
    println("RQ1")
    def createRow(data: List[ReductionData]): Unit =
      val originalSizes = data.map(r => r.origSize.toDouble)
      val avgOriginalSize = Statistics.mean(originalSizes)
      val stdOriginalSize = Statistics.stddev(originalSizes)

      val reducedSizes = data.map(r => r.reducedSize.toDouble)
      val avgReducedSize = Statistics.mean(reducedSizes)
      val stdReducedSize = Statistics.stddev(reducedSizes)

      val reductionPercentages = data.map(r => r.reductionPercentage)
      val avgReductionPercentage = Statistics.mean(reductionPercentages)
      val stdReductionPercentage = Statistics.stddev(reductionPercentages)

      println("number of programs reduced: " + originalSizes.size)
      println("avg original size: " + avgOriginalSize + " +-" + stdOriginalSize)
      println("avg reduced size: " + avgReducedSize + " +- " + stdReducedSize)
      println("avg reduction percetange: " + avgReductionPercentage + " +- " + stdReductionPercentage)

      println("########")
      reductionPercentages.foreach(s => println(s))
      println("########")

    createRow(baselineData)
    println("--------------------")
    createRow(transformingData)
    println("--------------------")

  def RQ2(baselineData: List[ReductionData], transformingData: List[ReductionData]): Unit =
    println("RQ2")
    def createRow(data: List[ReductionData]): Unit =
      val reductionTimes = data.map(r => r.reductionTime)
      val avgReductionTime = Statistics.mean(reductionTimes.map(_.toDouble))
      val stdReductionTime = Statistics.stddev(reductionTimes.map(_.toDouble))

      val oracleInvocations = data.map(r => r.interpreterTimes.size)
      val avgOracleInvocations = Statistics.mean(oracleInvocations.map(_.toDouble))
      val stdOracleInvocations = Statistics.stddev(oracleInvocations.map(_.toDouble))

      println("avg reduction time: " + avgReductionTime + " +- " + stdReductionTime)
      println("avg number of oracle invocations: " + avgOracleInvocations + " +- " + stdOracleInvocations)
      println("#########")

    createRow(baselineData)
    println("--------------------")
    createRow(transformingData)
    println("--------------------")

    val baselineReductionTimes = baselineData.map(r => r.reductionTime)
    val transformingReductionTimes = transformingData.map(r => r.reductionTime)
    val relativeSpeedRatios = transformingReductionTimes.zip(baselineReductionTimes).map(tpl => tpl._1.toDouble / tpl._2.toDouble)
    relativeSpeedRatios.foreach(println)

  def RQ3(baselineData: List[ReductionData],
          transformingData: List[ReductionData],
          countingData: List[ReductionData]): Unit =
    println("RQ3")

    def createRow(data: List[ReductionData]): Unit =
      val reductionTimes = data.map(r => r.reductionTime)
      val avgReductionTime = Statistics.mean(reductionTimes.map(_.toDouble))
      val stdReductionTime = Statistics.stddev(reductionTimes.map(_.toDouble))

      val oracleInvocations = data.map(r => r.interpreterTimes.size)
      val avgOracleInvocations = Statistics.mean(oracleInvocations.map(_.toDouble))
      val stdOracleInvocations = Statistics.stddev(oracleInvocations.map(_.toDouble))

      val interpreterTimes = data.map(r => r.interpreterTime)
      val avgInterpreterTime = Statistics.mean(interpreterTimes.map(_.toDouble))
      val stdInterpreterTime = Statistics.stddev(interpreterTimes.map(_.toDouble))

      val reductionPercentages = data.map(r => r.reductionPercentage)
      val avgReductionPercentage = Statistics.mean(reductionPercentages)
      val stdReductionPercentage = Statistics.stddev(reductionPercentages)

      println("avg reduction time: " + avgReductionTime + " +- " + stdReductionTime)
      println("avg number of oracle invocations: " + avgOracleInvocations + " +- " + stdOracleInvocations)
      println("avg time spent in the interpreter: " + avgInterpreterTime + " +- " + stdInterpreterTime)
      println("avg reduction percentage: " + avgReductionPercentage + " +- " + stdReductionPercentage)

    createRow(baselineData)
    println("--------------------")
    createRow(transformingData)
    println("--------------------")
    createRow(countingData)
    println("--------------------")

    val transformingReductionTimes = transformingData.map(r => r.reductionTime)
    val countingReductionTimes = countingData.map(r => r.reductionTime)
    val relativeSpeedRatios = transformingReductionTimes.zip(countingReductionTimes).map(tpl => tpl._1.toDouble / tpl._2.toDouble)
    relativeSpeedRatios.foreach(println)


  def RQ4(countingData: List[ReductionData],
          parallelData: List[ReductionData]): Unit =
    println("RQ4")

    def createRowTable1(data: List[ReductionData]): Unit =
      val reductionTimes = data.map(r => r.reductionTime)
      val avgReductionTime = Statistics.mean(reductionTimes.map(_.toDouble))
      val stdReductionTime = Statistics.stddev(reductionTimes.map(_.toDouble))

      val analysisTimes = data.map(r => r.analysisTime)
      val avgAnalysisTime = Statistics.mean(analysisTimes.map(_.toDouble))
      val stdAnalysisTime = Statistics.stddev(analysisTimes.map(_.toDouble))

      val interpreterTimes = data.map(r => r.interpreterTime)
      val avgInterpreterTime = Statistics.mean(interpreterTimes.map(_.toDouble))
      val stdInterpreterTime = Statistics.stddev(interpreterTimes.map(_.toDouble))

      println("avg reduction time: " + avgReductionTime + " +- " + stdReductionTime)
      println("avg time spent in the interpreter: " + avgInterpreterTime + " +- " + stdInterpreterTime)
      println("avg time spent in the analyser: " + avgAnalysisTime + " +- " + stdAnalysisTime)


    def createRowTable2(data: List[ReductionData]): Unit =
      val reductionTimes = data.map(r => r.reductionTime)
      val avgReductionTime = Statistics.mean(reductionTimes.map(_.toDouble))
      val stdReductionTime = Statistics.stddev(reductionTimes.map(_.toDouble))

      val oracleInvocations = data.map(r => r.interpreterTimes.size)
      val avgOracleInvocations = Statistics.mean(oracleInvocations.map(_.toDouble))
      val stdOracleInvocations = Statistics.stddev(oracleInvocations.map(_.toDouble))

      val reductionPercentages = data.map(r => r.reductionPercentage)
      val avgReductionPercentage = Statistics.mean(reductionPercentages)
      val stdReductionPercentage = Statistics.stddev(reductionPercentages)

      println("avg reduction time: " + avgReductionTime + " +- " + stdReductionTime)
      println("avg number of oracle invocations: " + avgOracleInvocations + " +- " + stdOracleInvocations)
      println("avg reduction percentage: " + avgReductionPercentage + " +- " + stdReductionPercentage)

    createRowTable1(countingData)
    println("------------------------------------------------------")

    createRowTable2(countingData)
    println("------------------------------------------------------")
    createRowTable2(parallelData)
    println("------------------------------------------------------")
    val ratios = parallelData.zip(countingData).map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
    ratios.foreach(println)
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    println("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    val zippedAnalysisCosts = countingData.flatMap(d => d.analysisTimes.map(tpl => (tpl._1, tpl._2.toDouble / d.origSize)))
    val zippedInterpreterCosts = countingData.flatMap(d => d.interpreterTimes.map(tpl => (tpl._1, tpl._2.toDouble / d.origSize)))
    val zippedOracleCosts = zippedAnalysisCosts.zip(zippedInterpreterCosts).map(tpl => (tpl._1._1 + tpl._2._1, tpl._2._2))
    zippedOracleCosts.foreach(tpl => println(tpl._2 + "\t" + tpl._1))
  */

  def RQ1(baselineData: List[ReductionData],
          transformingData: List[ReductionData],
          countingData: List[ReductionData],
          parallelData: List[ReductionData]): Unit =

    def createRow(data: List[ReductionData]): Unit =
      val reductionPercentages = data.map(r => r.reductionPercentage)
      val avgReductionPercentage = Statistics.median(reductionPercentages)
      val stdReductionPercentage = Statistics.stddev(reductionPercentages)

      println("median reduction %: " + avgReductionPercentage + " +- " + stdReductionPercentage)

    createRow(baselineData)
    createRow(transformingData)
    createRow(countingData)
    createRow(parallelData)

  def RQ2(baselineData: List[ReductionData],
          transformingData: List[ReductionData],
          countingData: List[ReductionData],
          parallelData: List[ReductionData]): Unit =
    def createRow(data: List[(ReductionData, ReductionData)]): Unit =
      val oracleRatio = data.map(tpl => tpl._1.interpreterTimes.size.toDouble / tpl._2.interpreterTimes.size)
      val avgOracleRatio = Statistics.median(oracleRatio)
      val stdOracleRatio = Statistics.stddev(oracleRatio)

      val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
      val avgReductionTimeRatio = Statistics.median(reductionTimeRatio)
      val stdReductionTimeRatio = Statistics.stddev(reductionTimeRatio)

      println("median oracle ratio: "+ avgOracleRatio + " +- " + stdOracleRatio)
      println("median reduction time ratio: " + avgReductionTimeRatio + " +- " + stdReductionTimeRatio)
      println("")

    def createBoxplot(data: List[(ReductionData, ReductionData)]): Unit =
      println("boxplot...")
      val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
      reductionTimeRatio.foreach(println)

    def createScatterPlot(): Unit =
      val zippedAnalysisCosts = countingData.flatMap(d => d.analysisTimes.map(tpl => (tpl._1, tpl._2.toDouble / d.origSize)))
      val zippedInterpreterCosts = countingData.flatMap(d => d.interpreterTimes.map(tpl => (tpl._1, tpl._2.toDouble / d.origSize)))
      val zippedOracleCosts = zippedAnalysisCosts.zip(zippedInterpreterCosts).map(tpl => (tpl._1._1 + tpl._2._1, tpl._2._2))
      zippedOracleCosts.foreach(tpl => println(tpl._2 + "\t" + tpl._1))

    createRow(baselineData.zip(baselineData))
    createRow(transformingData.zip(baselineData))
    createRow(countingData.zip(baselineData))
    createRow(parallelData.zip(baselineData))

    createBoxplot(transformingData.zip(baselineData))
    createBoxplot(transformingData.zip(countingData))
    createBoxplot(countingData.zip(parallelData))


object ReaderAndAnalyzeData {
  def main(args: Array[String]): Unit = {
    val baselineDataCollector: DataCollector = DataCollector.readObject("baselineDataCollector")
    val transformingDataCollector: DataCollector = DataCollector.readObject("transformingDataCollector")
    val countingDataCollector: DataCollector = DataCollector.readObject("countingDataCollector")
    val parallelDataCollector: DataCollector = DataCollector.readObject("parallelDataCollector")

    Evaluate.RQ1(
      baselineDataCollector.reductionData,
      transformingDataCollector.reductionData,
      countingDataCollector.reductionData,
      parallelDataCollector.reductionData
    )
    /*
    Evaluate.RQ2(
      baselineDataCollector.reductionData,
      transformingDataCollector.reductionData,
      countingDataCollector.reductionData,
      parallelDataCollector.reductionData
    )*/
  }
}

