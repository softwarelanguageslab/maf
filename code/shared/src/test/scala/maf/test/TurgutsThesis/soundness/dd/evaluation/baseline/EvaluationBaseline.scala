package maf.test.TurgutsThesis.soundness.dd.evaluation.baseline

import maf.test.TurgutsThesis.soundness.dd.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.evaluation.baseline.DDWithAllTransformationsEval
import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.{DDWithProfilingEval, DDWithoutProfilingEval}
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

trait EvaluationBaseline extends SchemeSoundnessWithDeltaDebuggingTests:

  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some((failureMsg, initAnalysisResults, _)) =>
          if failureMsg.nonEmpty then
            DDWithAllTransformationsEval.bugName = bugName
            DDWithAllTransformationsEval.reduce(program, this, benchmark, initAnalysisResults)
        case None =>
    }