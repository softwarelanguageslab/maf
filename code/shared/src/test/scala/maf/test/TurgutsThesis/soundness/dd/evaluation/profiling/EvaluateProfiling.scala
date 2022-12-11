package maf.test.TurgutsThesis.soundness.dd.evaluation.profiling

import maf.test.TurgutsThesis.soundness.dd.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.{DDWithProfilingEval, DDWithoutProfilingEval, ProfilingDataCollector}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

trait EvaluateProfiling extends SchemeSoundnessWithDeltaDebuggingTests:

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some((failureMsg, initAnalysisResults, _)) =>
          if failureMsg.nonEmpty then
            DDWithProfilingEval.reduce(program, this, benchmark, initAnalysisResults)
            DDWithoutProfilingEval.reduce(program, this, benchmark, initAnalysisResults)
        case None =>
    }