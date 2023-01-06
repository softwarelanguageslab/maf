package maf.test.TurgutsThesis.soundnessDD.evaluation.transforming

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.test.TurgutsThesis.soundnessDD.implementation.SoundnessDDTester
import maf.util.Reader

trait TransformingTester extends SoundnessDDTester {
  val bugName: String

  def runCompareAndtime(program: SchemeExp, benchmark: Benchmark): (Option[String], (Long, Long)) = {
    var evalStartTime: Long = 0
    var evalRunTime: Long = 0
    var evalEndTime: Long = 0

    var analysisStartTime: Long = 0
    var analysisRuntime: Long = 0
    var analysisEndTime: Long = 0

    var excThrown: Boolean = false

    var concreteResults: Option[Map[Identity, Set[Value]]] = None
    var anl: Option[Analysis] = None

    try
      evalStartTime = System.currentTimeMillis()
      concreteResults = Some(evalProgram(program, benchmark))
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
    else (Some(compareResults(anl.get, concreteResults.get)), (evalRunTime, analysisRuntime))
  }

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some(failureMsg) =>
          if failureMsg.nonEmpty then
            TransformingDD.bugName = bugName
            TransformingDD.reduce(program, this, benchmark)
        case _ =>
    }

}
