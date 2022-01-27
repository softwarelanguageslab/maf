package maf.test.modular.scheme

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.language.scheme._
import maf.modular._
import maf.modular.scv._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import maf.test._

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

trait ParallelSchemeModF extends SchemeModFSoundnessTests:
    def name = "parallel analysis (n = 8)"
    def analysis(program: SchemeExp) = new SimpleSchemeModFAnalysis(program)
      with SchemeConstantPropagationDomain
      with SchemeModFNoSensitivity
      with CallDepthFirstWorklistAlgorithm[SchemeExp]
      with ParallelWorklistAlgorithm[SchemeExp] {
      override def workers = 8
      override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with BigStepModFIntra with ParallelIntra
    }

trait ScvModF extends SchemeModFSoundnessTests:
    def name = "soft-contract verification soudness"
    def analysis(program: SchemeExp) =
        import maf.modular.scv.ScvSymbolicStore.given
        new ModAnalysis(program)
          with ScvBigStepSemantics
          with SchemeConstantPropagationDomain
          with StandardSchemeModFComponents
          with LIFOWorklistAlgorithm[SchemeExp]
          with SchemeModFSemanticsM
          with ScvOneContextSensitivity:
            override def intraAnalysis(cmp: Component) = new IntraScvSemantics(cmp)
            // we always return "unknown" here because the Z3 solver is not available in the `shared` module
            override val sat: ScvSatSolver[Value] = new ScvSatSolver[Value]():
                def sat(e: List[SchemeExp], vars: List[String]): IsSat[Value] = Unknown

// concrete test suites to run ...

class BigStepSchemeModFSoundnessTests extends BigStepSchemeModF with AllSequentialBenchmarks:
    override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b) || b.contains("infinite")

class SmallStepSchemeModFSoundnessTests extends SmallStepSchemeModF with AllSequentialBenchmarks:
    override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b)

class ParallelSchemeModFSoundnessTests extends ParallelSchemeModF with AllSequentialBenchmarks:
    override def isSlow(b: Benchmark) = true

// At the moment we check whether our extensions do not conflict with the soundness of regular Scheme programs (without contracts)
//class ScvModFTests extends ScvModF with AllSequentialBenchmarks:
//    override def isSlow(b: Benchmark) = !SchemeBenchmarkPrograms.various.contains(b)
