package maf.test.deltaDebugging.soundnessDD.variants.fitness

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait FitnessTester extends SoundnessDDTester {
  def bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("Fitness >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runCompareAndtime(program, benchmark) match
      case (Some(failureMsg), _) =>
        if failureMsg.nonEmpty then
          FitnessDD.bugName = bugName
          FitnessDD.reduce(program, this, benchmark)
      case _ =>

}
