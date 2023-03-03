package maf.test.deltaDebugging.soundnessDD.evaluation.deadCode

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, SchemeInterpreter}
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.evaluation.baseline.BaselineDD
import maf.test.deltaDebugging.soundnessDD.{SoundnessCountingDDTester, SoundnessDDTester}
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait DeadCodeTester extends SoundnessCountingDDTester {
  val bugName: String

  def runWithMaxStepsAndIdentifyDeadCode(program: SchemeExp, benchmark: Benchmark, maxSteps: Long):
  (Option[(Benchmark, Map[Identity, Set[Value]], Long)], (Long, Long)) =
    var evalStartTime: Long = 0
    var evalRunTime: Long = 0
    var evalEndTime: Long = 0

    var analysisStartTime: Long = 0
    var analysisRuntime: Long = 0
    var analysisEndTime: Long = 0

    var excThrown: Boolean = false

    var concreteResults: Option[Map[Identity, Set[Value]]] = None
    var evalSteps: Long = 0
    var anl: Option[Analysis] = None

    try
      evalStartTime = System.currentTimeMillis()
      val tpl = evalProgramWithMaxSteps(program, benchmark, maxSteps)
      concreteResults = Some(tpl._1)
      evalSteps = tpl._2
      evalEndTime = System.currentTimeMillis()
      evalRunTime = evalEndTime - evalStartTime
    catch case exc: Throwable =>
      excThrown = true
      evalEndTime = System.currentTimeMillis()
      evalRunTime = evalEndTime - evalStartTime

    try
      analysisStartTime = System.currentTimeMillis()
      anl = Some(runAnalysis(program, benchmark))
      analysisEndTime = System.currentTimeMillis()
      analysisRuntime = analysisEndTime - analysisStartTime
    catch case exc: Throwable =>
      excThrown = true
      analysisEndTime = System.currentTimeMillis()
      analysisRuntime = analysisEndTime - analysisStartTime

    if excThrown then
      (None, (evalRunTime, analysisRuntime))
    else (Some((compareResults(anl.get, concreteResults.get), concreteResults.get, evalSteps)), (evalRunTime, analysisRuntime))

  override def onBenchmark(benchmark: Benchmark): Unit =
    println("DeadCode >>> running benchmark: " + benchmark)
    // load the benchmark program
    val content = Reader.loadFile(benchmark)
    val program = parseProgram(content, benchmark)

    runWithMaxStepsAndIdentifyDeadCode(program, benchmark, Long.MaxValue) match
      case (Some((failureMsg, concreteResults, evalSteps)), _) =>
        if failureMsg.nonEmpty then
          val deadCodeRemoved = program.deleteChildren(exp => {
            !concreteResults.keySet.contains(exp.idn) /** Does not work as expected */
          })
          DeadCodeDD.maxSteps = evalSteps
          DeadCodeDD.bugName = bugName
          DeadCodeDD.reduce(program, this, benchmark)
      case _ =>
}
