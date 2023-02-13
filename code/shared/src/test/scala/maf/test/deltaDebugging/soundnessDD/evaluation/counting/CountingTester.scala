package maf.test.deltaDebugging.soundnessDD.evaluation.counting

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, SchemeInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.{SoundnessCountingDDTester, SoundnessDDTester}
import maf.test.deltaDebugging.soundnessDD.evaluation.baseline.BaselineDD
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait CountingTester extends SoundnessCountingDDTester {
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", SlowTest) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)
      runCompareAndtimeWithMaxSteps(program, benchmark, Long.MaxValue) match
        case (Some((failureMsg, evalSteps)), _) =>
          if failureMsg.nonEmpty then
            CountingDD.maxSteps = evalSteps
            CountingDD.bugName = bugName
            CountingDD.reduce(program, this, benchmark)
        case _ =>
    }
}
