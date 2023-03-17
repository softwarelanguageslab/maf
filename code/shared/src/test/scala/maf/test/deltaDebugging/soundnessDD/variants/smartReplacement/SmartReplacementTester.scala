package maf.test.deltaDebugging.soundnessDD.variants.smartReplacement

import maf.core.Identity
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester
import maf.util.Reader

trait SmartReplacementTester extends SoundnessDDTester {
  
  def bugName: String

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("SmartReplacement >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runAndCompare_(program, benchmark) match
      case Some((failureMsg, anlResults)) =>
        if failureMsg.nonEmpty then
          SmartReplacementDD.analysisResults = anlResults
          SmartReplacementDD.bugName = bugName
          Replacing.analysis = Some(analysis(program))
          SmartReplacementDD.reduce(program, this, benchmark)
      case _ =>
}
