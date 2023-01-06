package maf.test.TurgutsThesis.soundnessDD.evaluation.counting

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO, SchemeInterpreter}
import maf.test.TurgutsThesis.soundnessDD.evaluation.baseline.BaselineDD
import maf.test.TurgutsThesis.soundnessDD.implementation.SoundnessDDTester
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timer

trait CountingTester extends SoundnessDDTester {
  val bugName: String

  protected def runInterpreterWithMaxSteps(
                                i: SchemeInterpreter,
                                p: SchemeExp,
                                maxSteps: Long,
                              ): Value =
    i.runWithMaxSteps(p, maxSteps) // If there are code changes in the file, runs the "new" version by default (ensures compatibility with files containing changes).

  def evalProgramWithMaxSteps(program: SchemeExp, benchmark: Benchmark, maxSteps: Long): (Map[Identity, Set[Value]], Long) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time(runInterpreterWithMaxSteps(interpreter, program, maxSteps))
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

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
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
