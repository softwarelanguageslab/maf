package maf.test.TurgutsThesis.soundness.evaluation

import maf.test.TurgutsThesis.soundness.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.{DDWithProfilingEval, DDWithoutProfilingEval, ProfilingDataCollector}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

trait EvaluateProfiling extends SchemeSoundnessWithDeltaDebuggingTests:

  var onBenchmarkCount = 0
  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      onBenchmarkCount += 1
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some((failureMsg, initAnalysisResults, _)) =>
          if failureMsg.nonEmpty then
            DDWithProfilingEval.reduce(program, this, benchmark, initAnalysisResults)
            DDWithoutProfilingEval.reduce(program, this, benchmark, initAnalysisResults)
        case None =>

      if onBenchmarkCount == benchmarks.size then
        writeDataToFile()
    }

  def writeDataToFile(): Unit =
    val withProfilingDataCollector = DDWithProfilingEval.dataCollector
    val withoutProfilingDataCollector = DDWithoutProfilingEval.dataCollector

    withProfilingDataCollector.writeTo("withProfilingDataCollector")
    withoutProfilingDataCollector.writeTo("withoutProfilingDataCollector")