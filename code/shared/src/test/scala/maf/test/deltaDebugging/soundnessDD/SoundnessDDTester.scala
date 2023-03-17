package maf.test.deltaDebugging.soundnessDD

import maf.core.Identity
import maf.language.scheme.SchemeExp
import maf.language.scheme.interpreter.ConcreteValues.Value
import maf.language.scheme.interpreter.{ConcreteValues, FileIO}
import maf.modular.{AnalysisResults, ModAnalysis}
import maf.modular.scheme.SchemeDomain
import maf.test.SlowTest
import maf.test.deltaDebugging.soundnessDD.implementation.DD
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.{Duration, SECONDS}

trait SoundnessDDTester extends SchemeSoundnessTests:
  override def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(2, SECONDS))
  override def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(5, SECONDS)) //remember: concrete run may not halt

  protected def compareResults(
                                analysis: Analysis,
                                concreteResults: Map[Identity, Set[Value]],
                              ): String =
    val analysisResults = analysis.resultsPerIdn
    concreteResults.foreach { case (idn, concreteValues) =>
      val abstractValues = analysisResults.getOrElse(idn, Set.empty)
      concreteValues.foreach { concreteValue =>
        if !abstractValues.exists(checkSubsumption(analysis)(concreteValue, _)) then
          return
            s"""
               | Result at $idn is unsound:
               | - concrete value: $concreteValue
               | - abstract values: ${analysis.lattice.join(abstractValues)}
          """.stripMargin
      }
    }
    ""
  
  def evalProgram(program: SchemeExp, benchmark: Benchmark): Map[Identity, Set[Value]] =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time(runInterpreter(interpreter, program, timeout))
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    idnResults

  override def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
    // analyze the program using a ModF analysis
    val anl = analysis(program)
    val (ellapsed, timeout) = Timer.time(analysisTimeout(benchmark))
    SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = false)
    anl.analyzeWithTimeout(timeout)
    assume(anl.finished, "Analysis timed out")
    anl

  def runAndCompare(program: SchemeExp, benchmark: Benchmark): Option[String] = {
    try
      val concreteResults = evalProgram(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      Some(compareResults(anl, concreteResults))
    catch case exc: Throwable =>
      None
  }

  def runAndCompare_(program: SchemeExp, benchmark: Benchmark): Option[(String, Map[Identity, Any])] = {
    try
      val concreteResults = evalProgram(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      val analysisResults: Map[Identity, Any] = anl.resultsPerIdn
      Some((compareResults(anl, concreteResults), analysisResults))
    catch case exc: Throwable =>
      None
  }

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
