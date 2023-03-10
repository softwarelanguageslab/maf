package maf.test.deltaDebugging.soundnessDD.variants

import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.variants.baseline.{BaselineDD, SaveBaseline}
import maf.test.deltaDebugging.soundnessDD.variants.counting.{CountingDD, SaveCounting}
import maf.test.deltaDebugging.soundnessDD.variants.deadCode.{DeadCodeDD, SaveDeadCode}
import maf.test.deltaDebugging.soundnessDD.variants.fitness.{FitnessDD, SaveFitness}
import maf.test.deltaDebugging.soundnessDD.variants.parallel.{ParallelDD, SaveParallel}
import maf.test.deltaDebugging.soundnessDD.variants.profiling.{ProfilingDD, SaveProfiling}
import maf.test.deltaDebugging.soundnessDD.variants.transforming.{SaveTransforming, SchemeModFLocalAdaptiveTests1, TransformingDD}
import maf.util.benchmarks.Statistics

import java.io.PrintWriter

object Evaluate:
  def main(args: Array[String]): Unit = {
    /*
    for(i <- List(1, 2, 3)) //JIT warm-up
      SaveCounting.save()
      SaveParallel.save()
      SaveProfiling.save()
      SaveDeadCode.save()
      SaveBaseline.save()
      SaveTransforming.save()
      SaveFitness.save()
    */

    CountingDD.dataCollector = new DataCollector
    BaselineDD.dataCollector = new DataCollector
    FitnessDD.dataCollector = new DataCollector
    TransformingDD.dataCollector = new DataCollector
    ParallelDD.dataCollector = new DataCollector
    ProfilingDD.dataCollector = new DataCollector
    DeadCodeDD.dataCollector = new DataCollector

    for(i <- List(1))
      SaveCounting.save()
      SaveParallel.save()
      SaveProfiling.save()
      SaveDeadCode.save()
      SaveBaseline.save()
      SaveTransforming.save()
      SaveFitness.save()
  }

  def save(tests: List[SoundnessDDTester], dataCollectorString: String, dataCollector: DataCollector): Unit = {
    println("writing to disk")
    dataCollector.writeTo(dataCollectorString)
  }

  def RQ1(baselineData: List[ReductionData],
          transformingData: List[ReductionData],
          countingData: List[ReductionData],
          parallelData: List[ReductionData],
          profilingData: List[ReductionData],
          deadCodeData: List[ReductionData],
          randomData: List[ReductionData] //random-order transformations
         ): Unit =

    def createRow(data: List[ReductionData]): Unit =
      val reductionPercentages = data.map(r => r.reductionPercentage)
      val avgReductionPercentage = Statistics.median(reductionPercentages)
      val stdReductionPercentage = Statistics.stddev(reductionPercentages)

      println("median reduction %: " + avgReductionPercentage + " +- " + stdReductionPercentage)

    def createBoxplot(data: List[ReductionData], path: String): Unit =
      println("boxplot...")
      val reductionPercentages = data.map(d => d.reductionPercentage)
      val asString: String = reductionPercentages.map(t => t.toString + "\n").fold("")((t1, t2) => t1 + t2)
      val pw = PrintWriter(path)
      pw.write(asString)
      pw.close()

    println("RQ1")
    createRow(baselineData)
    createRow(transformingData)
    createRow(countingData)

    createBoxplot(baselineData, "/Users/turgut/Desktop/cs5/thesis/evaluations/data/Space/ladd.txt")
    createBoxplot(transformingData, "/Users/turgut/Desktop/cs5/thesis/evaluations/data/Space/ssdd.txt")
    createBoxplot(countingData, "/Users/turgut/Desktop/cs5/thesis/evaluations/data/Space/counting.txt")


  def RQ2(baselineData: List[ReductionData],
          transformingData: List[ReductionData],
          countingData: List[ReductionData],
          parallelData: List[ReductionData],
          profilingData: List[ReductionData],
          deadCodeData: List[ReductionData],
          randomData: List[ReductionData]): Unit =

    /*
    def createRow(data: List[(ReductionData, ReductionData)], rowName: String): Unit =
      val oracleRatio = data.map(tpl => tpl._1.oracleTreeSizes.size.toDouble / tpl._2.oracleTreeSizes.size)
      val avgOracleRatio = Statistics.median(oracleRatio)
      val stdOracleRatio = Statistics.stddev(oracleRatio)

      val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
      val avgReductionTimeRatio = Statistics.median(reductionTimeRatio)
      val stdReductionTimeRatio = Statistics.stddev(reductionTimeRatio)

      println(rowName)
      println("median oracle ratio: "+ avgOracleRatio + " +- " + stdOracleRatio)
      println("median reduction time ratio: " + avgReductionTimeRatio + " +- " + stdReductionTimeRatio)

    def createBoxplot(data: List[(ReductionData, ReductionData)], path: String): Unit =
      println("boxplot...")
      val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
      val asString: String = reductionTimeRatio.map(t => t.toString + "\n").fold("")((t1, t2) => t1 + t2)
      val pw = PrintWriter(path)
      pw.write(asString)
      pw.close()
    */

    def createRow(data: List[ReductionData], rowName: String): Unit =
      val oracleCount = data.map(p => p.oracleTreeSizes.size.toDouble)
      val avgOracleCount = Statistics.median(oracleCount)
      val stdOracleCount = Statistics.stddev(oracleCount)

      val reductionTime = data.map(p => p.reductionTime.toDouble)
      val avgReductionTime = Statistics.median(reductionTime)
      val stdReductionTime = Statistics.stddev(reductionTime)

      println(rowName)
      println("median oracle ratio: " + avgOracleCount + " +- " + stdOracleCount)
      println("median reduction time ratio: " + avgReductionTime + " +- " + stdReductionTime)

    def createBoxplot(data: List[(ReductionData, ReductionData)], path: String): Unit =
      println("boxplot...")
      val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
      val asString: String = reductionTimeRatio.map(t => t.toString + "\n").fold("")((t1, t2) => t1 + t2)
      val pw = PrintWriter(path)
      pw.write(asString)
      pw.close()

    println("RQ2")
    createRow(baselineData, "baseline")
    createRow(randomData, "random")
    createRow(transformingData, "transforming")
    createRow(countingData, "counting")
    createRow(profilingData, "profiling")
    createRow(deadCodeData, "deadcode")
    createRow(parallelData, "parallel")

    createBoxplot(randomData.zip(transformingData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/OrderedVsRandom/ratio.txt")
    createBoxplot(transformingData.zip(baselineData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/SSDDVsLADD/ratio.txt")
    createBoxplot(transformingData.zip(countingData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/SSDDVsCounting/ratio.txt")
    createBoxplot(transformingData.zip(parallelData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/SSDDVsParallelism/ratio.txt")
    createBoxplot(transformingData.zip(profilingData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/SSDDVsProfiling/ratio.txt")
    createBoxplot(transformingData.zip(deadCodeData), "/Users/turgut/Desktop/cs5/thesis/evaluations/data/SSDDVsDeadcode/ratio.txt")



object ReaderAndAnalyzeData {
  def main(args: Array[String]): Unit = {
    val baselineDataCollector: DataCollector = DataCollector.readObject("baselineDataCollector")
    val transformingDataCollector: DataCollector = DataCollector.readObject("transformingDataCollector")
    val countingDataCollector: DataCollector = DataCollector.readObject("countingDataCollector")
    val parallelDataCollector: DataCollector = DataCollector.readObject("parallelDataCollector")
    val profilingDataCollector: DataCollector = DataCollector.readObject("profilingDataCollector")
    val deadCodeDataCollector: DataCollector = DataCollector.readObject("deadCodeDataCollector")
    val randomDatacollector: DataCollector = DataCollector.readObject("randomDataCollector")


    Evaluate.RQ1(
      baselineDataCollector.reductionData,
      transformingDataCollector.reductionData,
      countingDataCollector.reductionData,
      parallelDataCollector.reductionData,
      profilingDataCollector.reductionData,
      deadCodeDataCollector.reductionData,
      randomDatacollector.reductionData
    )


    Evaluate.RQ2(
      baselineDataCollector.reductionData,
      transformingDataCollector.reductionData,
      countingDataCollector.reductionData,
      parallelDataCollector.reductionData,
      profilingDataCollector.reductionData,
      deadCodeDataCollector.reductionData,
      randomDatacollector.reductionData
    )
  }
}

