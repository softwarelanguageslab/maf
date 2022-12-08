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
        writeDataToFile()
    }

  def writeDataToFile(): Unit =
    val withProfilingDataCollector = DDWithProfilingEval.dataCollector
    val withoutProfilingDataCollector = DDWithoutProfilingEval.dataCollector

    withProfilingDataCollector.writeTo("withProfilingDataCollector")
    withoutProfilingDataCollector.writeTo("withoutProfilingDataCollector")