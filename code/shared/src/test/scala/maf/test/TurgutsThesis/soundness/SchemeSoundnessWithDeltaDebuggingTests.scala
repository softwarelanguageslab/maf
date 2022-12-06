package maf.test.TurgutsThesis.soundness

import maf.TurgutsThesis.gtr.GTR
import maf.TurgutsThesis.gtr.transformations.*
import maf.TurgutsThesis.gtr.variants.{CountingGTR, FirstInternalGTR, JumpyGTR, SimpleGTR}
import maf.core.{Identity, NoCodeIdentity, Position}
import maf.language.CScheme.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.*
import maf.language.scheme.interpreter.ConcreteValues.*
import maf.language.scheme.lattices.SchemeOp
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.test.TurgutsThesis.soundness.dd.evaluation.DDWithProfilingEval
import maf.test.TurgutsThesis.soundness.dd.{DDWithAllTransformations, DDWithProfiling, DDWithoutProfiling}
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.{Timeout, Timer}

import scala.concurrent.duration.{Duration, MINUTES, SECONDS}

trait SchemeSoundnessWithDeltaDebuggingTests extends SchemeSoundnessTests:
  override def analysisTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(5, SECONDS))
  override def concreteTimeout(b: Benchmark): Timeout.T = Timeout.start(Duration(2, SECONDS)) //remember: concrete run may not halt

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

  def evalConcreteWithSteps(program: SchemeExp, benchmark: Benchmark): (Int, Map[Identity, Set[Value]]) =
    var idnResults = Map[Identity, Set[Value]]().withDefaultValue(Set())
    val timeout = concreteTimeout(benchmark)
    val times = concreteRuns(benchmark)
    var evalSteps = 0
    val addResult: (Identity, ConcreteValues.Value) => Unit = (i, v) => idnResults += (i -> (idnResults(i) + v))
    val interpreter = createInterpreter(addResult, io = new FileIO(Map("input.txt" -> "foo\nbar\nbaz", "output.txt" -> "")), benchmark)
    for _ <- 1 to times do
      val (ellapsed, _) = Timer.time(runInterpreter(interpreter, program, timeout))
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = true)
    evalSteps += interpreter.getEvalSteps()
    (evalSteps, idnResults)

  override def runAnalysis(program: SchemeExp, benchmark: Benchmark): Analysis =
      // analyze the program using a ModF analysis
      val anl = analysis(program)
      val (ellapsed, timeout) = Timer.time(analysisTimeout(benchmark))
      SchemeSoundnessTests.logEllapsed(this, benchmark, ellapsed, concrete = false)
      anl.analyzeWithTimeout(timeout)
      assume(anl.finished, "Analysis timed out")
      anl

  def runAndCompare(program: SchemeExp, benchmark: Benchmark): Option[(String, Array[(String, Int)], Int)] = {
    try
      val (evalSteps, concreteResults) = evalConcreteWithSteps(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      Some(compareResults(anl, concreteResults),
            anl.asInstanceOf[SequentialWorklistAlgorithm[SchemeExp]].getReAnalysisMap().toArray.sortWith((tpl1, tpl2) => tpl1._2 > tpl2._2),
            evalSteps)
    catch case exc: Throwable =>
        None
    }

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      runAndCompare(program, benchmark) match
        case Some((failureMsg, analysisResults, evalSteps)) =>
          if failureMsg.nonEmpty then
            DDWithProfiling.reduce(program, this, benchmark, analysisResults)
        case _ =>     
    }