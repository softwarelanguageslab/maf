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
import maf.test.TurgutsThesis.soundness.dd.{DDWithAllTransformations, DDWithProfiling, DDWithoutProfiling}
import maf.test.modular.scheme.SchemeSoundnessTests
import maf.util.Reader
import maf.util.benchmarks.Timeout

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

  def runAndCompare(program: SchemeExp, benchmark: Benchmark): (String, Array[(String, Int)]) = {
    try { // run the program using a concrete interpreter
      val concreteResults = evalConcrete(program, benchmark)
      // analyze the program using a ModF analysis
      val anl = runAnalysis(program, benchmark)
      // check if the analysis results soundly (over-)approximate the concrete results
      (compareResults(anl, concreteResults),
        anl.asInstanceOf[SequentialWorklistAlgorithm[SchemeExp]].getReAnalysisMap().toArray.sortWith((tpl1, tpl2) => tpl1._2 > tpl2._2))
    }
    catch {
      case _: Throwable =>
        ("", Array())
    }
  }

  override def onBenchmark(benchmark: Benchmark): Unit =
    property(s"Analysis of $benchmark using $name is sound.", testTags(benchmark): _*) {
      // load the benchmark program
      val content = Reader.loadFile(benchmark)
      val program = parseProgram(content, benchmark)

      val (failureMsg, analysisResults) = runAndCompare(program, benchmark)
      if failureMsg.nonEmpty then
        DDWithAllTransformations.reduce(program, this, benchmark, analysisResults)
    }