package maf.test.deltaDebugging.soundnessDD.variants.parallel

import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait ParallelTester extends SoundnessDDTester {
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Parallel >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)
    runCompareAndtime(program, benchmark) match
      case (Some(failureMsg), _) =>
        if failureMsg.nonEmpty then
          ParallelDD.bugName = bugName
          ParallelDD.reduce(program, this, benchmark)
      case _ =>
}
