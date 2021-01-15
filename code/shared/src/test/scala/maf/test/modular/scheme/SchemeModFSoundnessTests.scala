package maf.test.modular.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.core.Position._
import maf.language.scheme._
import maf.modular._
import maf.modular.adaptive._
import maf.modular.adaptive.scheme._
import maf.modular.adaptive.scheme.adaptiveArgumentSensitivity._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist.{LIFOWorklistAlgorithm, ParallelWorklistAlgorithm}
import maf.test._

trait SchemeModFSoundnessTests extends SchemeSoundnessTests {
  override def testTags(b: Benchmark) = super.testTags(b) :+ SchemeModFTest
}

trait BigStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "big-step semantics"
  def analysis(
      program: SchemeExp
    ) = new SimpleSchemeModFAnalysis(program) with SchemeConstantPropagationDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]
}

trait BigStepSchemeModFPrimCSSensitivity extends SchemeModFSoundnessTests {
  def name = "big-step semantics with call-site sensitivity for primitives"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
    with SchemeConstantPropagationDomain
    with SchemeModFCompoundSensitivities.TrackLowToHighSensitivity.S_CS_0
    with LIFOWorklistAlgorithm[SchemeExp]
}

trait SmallStepSchemeModF extends SchemeModFSoundnessTests {
  def name = "small-step semantics"
  def analysis(program: SchemeExp) = new ModAnalysis(program)
    with SchemeModFSemantics
    with SmallStepModFSemantics
    with StandardSchemeModFComponents
    with SchemeConstantPropagationDomain
    with SchemeModFNoSensitivity
    with LIFOWorklistAlgorithm[SchemeExp] {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with SmallStepIntra
  }
}

trait ParallelSchemeModF extends SchemeModFSoundnessTests {
  def name = "parallel analysis (n = 8)"
  def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
    with SchemeConstantPropagationDomain
    with SchemeModFNoSensitivity
    with ParallelWorklistAlgorithm[SchemeExp] {
    override def workers = 8
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
  }
}

trait SimpleAdaptiveSchemeModF extends SchemeModFSoundnessTests {
  def name = "simple adaptive argument sensitivity (limit = 5)"
  def analysis(program: SchemeExp) = new AdaptiveModAnalysis(program)
    with AdaptiveArgumentSensitivityPolicy3
    with AdaptiveSchemeModFSemantics
    with SchemeConstantPropagationDomain
    with LIFOWorklistAlgorithm[SchemeExp] {
    val limit = 5
    override def allocCtx(
        nam: Option[String],
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ) = super.allocCtx(nam, clo, args, call, caller)
    override def updateValue(update: Component => Component)(v: Value) = super.updateValue(update)(v)
  }
}

// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.other.contains(b)
}

class BigStepSchemeModFPrimCSSensitivitySoundnessTests extends BigStepSchemeModFPrimCSSensitivity with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = true
}

class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.other.contains(b)
}

class ParallelSchemeModFSoundnessTests extends ParallelSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.other.contains(b)
}
