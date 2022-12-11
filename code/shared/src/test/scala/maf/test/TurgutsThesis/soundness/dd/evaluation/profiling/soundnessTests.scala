package maf.test.TurgutsThesis.soundness.dd.evaluation.profiling

import maf.core.Position
import maf.language.scheme.primitives.SchemePrelude
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.*
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.EvaluateProfiling
import maf.test.TurgutsThesis.soundness.SchemeModFLocalSoundnessTests
import maf.test.TurgutsThesis.soundness.dd.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.{AllBenchmarks, AllSequentialBenchmarks, RandomSequentialBenchmarks, VariousSequentialBenchmarks}
import maf.test.TurgutsThesis.soundnessBugs.*

trait SchemeModFLocalSoundnessTests extends EvaluateProfiling:

  override def benchmarks: Set[Benchmark] = Set(
    "/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/test/R5RS/scp1/flatten.scm",
    "/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/test/R5RS/scp1/sim-fast-multiply.scm"
  )

  override def parseProgram(txt: String, benchmark: String): SchemeExp =
    val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
    val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.rename(SchemeParser.undefine(transf))

class EvalProfilingTestSuiteA extends SchemeModFLocalSoundnessTests:
  def n = 100
  def name = s"MODF LOCAL w/ ASW -- policy A (n = $n)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyA(n)
      with BeginBug