package maf.test.modular.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
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
    with CallDepthFirstWorklistAlgorithm[SchemeExp]
    with ParallelWorklistAlgorithm[SchemeExp] {
    override def workers = 8
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
  }
}

// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b)
}

class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b)
}

class ParallelSchemeModFSoundnessTests extends ParallelSchemeModF with AllSequentialBenchmarks {
  override def isSlow(b: Benchmark) = true
}
