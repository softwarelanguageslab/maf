package maf.test.deltaDebugging.soundnessDD

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, CountingSchemeInterpreter, FileIO, IO, SchemeInterpreter}
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.benchmarks.Timer

trait SoundnessCountingDDTester extends SoundnessDDTester:
  override def createInterpreter(addResult: (Identity, Value) => Unit, io: IO, benchmark: Benchmark): CountingSchemeInterpreter =
    new CountingSchemeInterpreter(addResult, io)

  def evalProgramWithMaxSteps(program: SchemeExp, benchmark: Benchmark, maxSteps: Long): (Map[Identity, Set[Value]], Long) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter: CountingSchemeInterpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time(interpreter.runWithMaxSteps(program, timeout, maxSteps))
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    (idnResults, interpreter.getEvalSteps())

  def runCompareAndtimeWithMaxSteps(program: SchemeExp, benchmark: Benchmark, maxSteps: Long): (Option[(String, Long)], (Long, Long)) = {
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
    else (Some((compareResults(anl.get, concreteResults.get), evalSteps)), (evalRunTime, analysisRuntime))
  }
