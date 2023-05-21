package maf.test.deltaDebugging.soundnessDD.variants

import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.test.deltaDebugging.soundnessDD.variants.baseline.{BaselineDD, SaveBaseline}
import maf.test.deltaDebugging.soundnessDD.variants.counting.{CountingDD, SaveCounting}
import maf.test.deltaDebugging.soundnessDD.variants.killLambda.{KillLambdaDD, SaveKillLambda}
import maf.test.deltaDebugging.soundnessDD.variants.fitness.{FitnessDD, SaveFitness}
import maf.test.deltaDebugging.soundnessDD.variants.parallel.{ParallelDD, SaveParallel}
import maf.test.deltaDebugging.soundnessDD.variants.smartReplacement.SaveSmartReplacement
import maf.test.deltaDebugging.soundnessDD.variants.transforming.{SaveTransforming, SchemeModFLocalAdaptiveTests1, TransformingDD}
import maf.util.benchmarks.Statistics

import java.io.PrintWriter

object Evaluate:
  def main(args: Array[String]): Unit = {
    def reset(): Unit = {
      FitnessDD.dataCollector = new DataCollector
      BaselineDD.dataCollector = new DataCollector
      TransformingDD.dataCollector = new DataCollector
      CountingDD.dataCollector = new DataCollector
      ParallelDD.dataCollector = new DataCollector
      System.gc()
    }

    val warmupIterations = 1
    val dataIterations = 5

    /*
    Collected:
      - Fitness
      - Baseline
      - Parallel
      - Counting
    */

    for(i <- 1 to warmupIterations) {
      SaveTransforming.save()
    }

    println("JIT warm-up done")

    reset()

    for (i <- 1 to dataIterations) {
      SaveTransforming.save()
    }

    reset()
  }

  def save(tests: List[SoundnessDDTester], dataCollectorString: String, dataCollector: DataCollector): Unit = {
    println("writing to disk")
    dataCollector.writeTo(dataCollectorString)
  }

  def sizeCreateRow(data: List[ReductionData]): Unit =
    val reductionPercentages = data.map(r => r.reductionPercentage)
    val avgReductionPercentage = Statistics.median(reductionPercentages)
    val stdReductionPercentage = Statistics.stddev(reductionPercentages)

    println("median reduction %: " + avgReductionPercentage + " +- " + stdReductionPercentage)


  def sizeCreateBoxplot(data: List[ReductionData], path: String): Unit =
    println("boxplot...")
    val reductionPercentages = data.map(d => d.reductionPercentage)
    val asString: String = reductionPercentages.map(t => t.toString + "\n").fold("")((t1, t2) => t1 + t2)
    val pw = PrintWriter(path)
    pw.write(asString)
    pw.close()

  def timeCreateRow(data: List[ReductionData], rowName: String): Unit =
    val oracleCount = data.map(p => p.oracleTreeSizes.size.toDouble)
    val avgOracleCount = Statistics.median(oracleCount)
    val stdOracleCount = Statistics.stddev(oracleCount)

    val reductionTime = data.map(p => p.reductionTime.toDouble)
    val avgReductionTime = Statistics.median(reductionTime)
    val stdReductionTime = Statistics.stddev(reductionTime)

    println(rowName)
    println("median oracle ratio: " + avgOracleCount + " +- " + stdOracleCount)
    println("median reduction time ratio: " + avgReductionTime + " +- " + stdReductionTime)

  def timeCreateBoxplot(data: List[(ReductionData, ReductionData)], path: String): Unit =
    println("boxplot...")
    val reductionTimeRatio = data.map(tpl => tpl._1.reductionTime.toDouble / tpl._2.reductionTime)
    val asString: String = reductionTimeRatio.map(t => t.toString + "\n").fold("")((t1, t2) => t1 + t2)
    val pw = PrintWriter(path)
    pw.write(asString)
    pw.close()

  def RQ1(baselineData: List[ReductionData],
          randomData: List[ReductionData]): Unit =
    println("RQ1: output size, Scheme-Reduce vs GTR*")
    sizeCreateRow(baselineData)
    sizeCreateRow(randomData)

    sizeCreateBoxplot(baselineData, "/Users/turgut/Desktop/cs5/thesis/evaluations/final/RQ1/GTR.txt")
    sizeCreateBoxplot(randomData, "/Users/turgut/Desktop/cs5/thesis/evaluations/final/RQ1/SchemeReduce.txt")

  def RQ2(baselineDD: List[ReductionData], randomData: List[ReductionData]): Unit =
    println("RQ2: time, Scheme-Reduce vs GTR*")

    timeCreateRow(baselineDD, "baseline")
    timeCreateRow(randomData, "random")

    timeCreateBoxplot(randomData.zip(baselineDD), "/Users/turgut/Desktop/cs5/thesis/evaluations/final/RQ2/runtimes.txt")

  def RQ3(randomData: List[ReductionData], transformingData: List[ReductionData]): Unit =
    println("RQ3: time, Ordered Scheme-Reduce vs Scheme-Reduce")

    timeCreateRow(randomData, "Scheme-Reduce")
    timeCreateRow(transformingData, "Ordered Scheme-Reduce")

    timeCreateBoxplot(randomData.zip(transformingData), "/Users/turgut/Desktop/cs5/thesis/evaluations/final/RQ3/runtimes.txt")


object ReaderAndAnalyzeData {
  def main(args: Array[String]): Unit = {
    val baselineDataCollector: DataCollector = DataCollector.readObject("baselineDataCollector")
    merge(baselineDataCollector)
    val randomDatacollector: DataCollector = DataCollector.readObject("randomDataCollector")
    merge(randomDatacollector)
    val transformingDataCollector: DataCollector = DataCollector.readObject("transformingDataCollector")
    merge(transformingDataCollector)
    val countingDataCollector: DataCollector = DataCollector.readObject("countingDataCollector")
    merge(countingDataCollector)
    val parallelDataCollector: DataCollector = DataCollector.readObject("parallelDataCollector")
    merge(parallelDataCollector)

    Evaluate.RQ1(baselineDataCollector.reductionData, randomDatacollector.reductionData)
    Evaluate.RQ2(baselineDataCollector.reductionData, randomDatacollector.reductionData)
    Evaluate.RQ3(randomDatacollector.reductionData, transformingDataCollector.reductionData)
  }

  /** Merge aggregates several (non-deterministic) time measurements, to increase confidence in our results */
  def merge(collector: DataCollector): Unit =
    val grouped = collector.reductionData.groupBy(d => (d.bugName, d.benchmark))
    val it = grouped.values
    var aggregatedList: List[ReductionData] = List()
    for (i <- it) {
      val first = i.head
      val agg = i.tail.fold(first)((r1, r2) => {
        ReductionData(r1.benchmark, r1.bugName, r1.origSize, r1.reducedSize, r1.reductionPercentage, r1.reductionTime + r2.reductionTime, r1.oracleTreeSizes)
      })
      val divideByLength = ReductionData(
        agg.benchmark, agg.bugName, agg.origSize, agg.reducedSize, agg.reductionPercentage, agg.reductionTime / i.length, agg.oracleTreeSizes
      )
      aggregatedList = aggregatedList.::(divideByLength)
    }

    collector.reductionData = aggregatedList
}

