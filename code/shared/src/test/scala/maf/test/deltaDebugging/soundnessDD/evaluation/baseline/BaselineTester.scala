package maf.test.deltaDebugging.soundnessDD.evaluation.baseline

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait BaselineTester extends SoundnessDDTester {
  
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Baseline >>> running benchmark: " + benchmark)
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runCompareAndtime(program, benchmark) match
        case (Some(failureMsg), _) =>
          if failureMsg.nonEmpty then
            BaselineDD.bugName = bugName
            BaselineDD.reduce(program, this, benchmark)
        case _ =>
}
