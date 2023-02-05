package maf.test.deltaDebugging.soundnessDD.evaluation.parallel

import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessCountingDDTester
import maf.util.Reader

trait ParallelTester extends SoundnessCountingDDTester {
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", SlowTest) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)
      runCompareAndtimeWithMaxSteps(program, benchmark, Long.MaxValue) match
        case (Some((failureMsg, evalSteps)), _) =>
          if failureMsg.nonEmpty then
            ParallelDD.maxSteps = evalSteps
            ParallelDD.bugName = bugName
            ParallelDD.reduce(program, this, benchmark)
        case _ =>
    }
}
