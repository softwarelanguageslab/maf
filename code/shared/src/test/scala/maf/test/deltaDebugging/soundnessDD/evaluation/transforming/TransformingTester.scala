package maf.test.deltaDebugging.soundnessDD.evaluation.transforming

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait TransformingTester extends SoundnessDDTester {
  val bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Transforming >>> running benchmark: " + benchmark)
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runCompareAndtime(program, benchmark) match
        case (Some(failureMsg), times) =>
          if failureMsg.nonEmpty then
            TransformingDD.bugName = bugName
            TransformingDD.reduce(program, this, benchmark, times)
        case _ =>

}
