package maf.test.TurgutsThesis.soundness.evaluation

import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.evaluation.{DDWithProfilingEval, DDWithoutProfilingEval, DataCollector}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

trait EvaluateSoundnessTests extends SchemeSoundnessWithDeltaDebuggingTests:

  var onBenchmarkCount = 0
  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      onBenchmarkCount += 1
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some((failureMsg, analysisResults, _)) =>
          if failureMsg.nonEmpty then
            DDWithProfilingEval.reduce(program, this, benchmark, analysisResults)
            DDWithoutProfilingEval.reduce(program, this, benchmark, analysisResults)
        case None =>

      if onBenchmarkCount == benchmarks.size then
        evaluateData()
     }

  def evaluateData(): Unit =
    def evaluateDataCollector(dataCollector: DataCollector): Unit =
      val averageReductionTime = dataCollector.reductionTimes.sum / dataCollector.reductionTimes.length
      val averageOracleCount = dataCollector.oracleCounts.sum / dataCollector.oracleCounts.length
      val averageOriginalProgramSize = dataCollector.programSizes.sum / dataCollector.programSizes.length
      val averageReducedProgramSize = dataCollector.reducedSizes.sum / dataCollector.reducedSizes.length

      //analysis steps:
      val averageAnalysisSteps = dataCollector.analysisSteps.map(_._1).sum / dataCollector.analysisSteps.length
      val averageAnalysisStepsAfter1Reductions = dataCollector.analysisSteps.filter(_._2 == 1).map(_._1).sum / dataCollector.analysisSteps.length
      val averageAnalysisStepsAfter2Reductions = dataCollector.analysisSteps.filter(_._2 == 2).map(_._1).sum / dataCollector.analysisSteps.length

      //interpreter steps:
      val averageInterpreterSteps = dataCollector.interpreterSteps.map(_._1).sum / dataCollector.interpreterSteps.length
      val averageInterpreterStepsAfter1Reductions = dataCollector.interpreterSteps.filter(_._2 == 1).map(_._1).sum / dataCollector.interpreterSteps.length
      val averageInterpreterStepsAfter2Reductions = dataCollector.interpreterSteps.filter(_._2 == 2).map(_._1).sum / dataCollector.interpreterSteps.length

      println("number of programs: " + dataCollector.reducedSizes.length)
      println("average reduction time: " + averageReductionTime)
      println("average oracle count: " + averageOracleCount)
      println("average original program size: " + averageOriginalProgramSize)
      println("average reduced program size: " + averageReducedProgramSize)

      println("#####")

      println("average analysis steps: " + averageAnalysisSteps)
      println("average analysis steps after 1 reductions: " + averageAnalysisStepsAfter1Reductions)
      println("average analysis steps after 2 reductions: " + averageAnalysisStepsAfter2Reductions)

      println("average interpreter steps: " + averageInterpreterSteps)
      println("average interpreter steps after 1 reductions: " + averageInterpreterStepsAfter1Reductions)
      println("average interpreter steps after 2 reductions: " + averageInterpreterStepsAfter2Reductions)

    val withProfilingDataCollector = DDWithProfilingEval.dataCollector
    val withoutProfilingDataCollector = DDWithoutProfilingEval.dataCollector

    println(">>>>> results WITHOUT profiling <<<<< ")
    evaluateDataCollector(withoutProfilingDataCollector)
    println("")
    println("")
    println("####################################")
    println("")
    println("")
    println(">>>>> results WITH profiling <<<<< ")
    evaluateDataCollector(withProfilingDataCollector)
