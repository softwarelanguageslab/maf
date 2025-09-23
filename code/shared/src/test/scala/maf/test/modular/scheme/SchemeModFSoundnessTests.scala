package maf.test.modular.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme._
import maf.language.symbolic.lattices.*
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.test._
import maf.language.symbolic.Formula

trait SchemeModFSoundnessTests extends SchemeSoundnessTests:
    override def testTags(b: Benchmark) = super.testTags(b) :+ SchemeModFTest

trait BigStepSchemeModF extends SchemeModFSoundnessTests:
    def name = "big-step semantics"
    def analysis(
        program: SchemeExp
      ) = new SimpleSchemeModFAnalysis(program) with SchemeConstantPropagationDomain with SchemeModFNoSensitivity with LIFOWorklistAlgorithm[SchemeExp]

trait SmallStepSchemeModF extends SchemeModFSoundnessTests:
    def name = "small-step semantics"
    def analysis(program: SchemeExp) = new ModAnalysis(program)
        with SchemeModFSemanticsM
        with SmallStepModFSemantics
        with StandardSchemeModFComponents
        with SchemeConstantPropagationDomain
        with SchemeModFNoSensitivity
        with LIFOWorklistAlgorithm[SchemeExp] {
        override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with SmallStepIntra
    }


// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with AllSequentialBenchmarks:
    override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b) || b.contains("infinite")

class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with AllSequentialBenchmarks:
    override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b)