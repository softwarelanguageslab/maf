package maf.test.TurgutsThesis.soundness.dd.evaluation.baseline

import maf.core.Position
import maf.language.scheme.primitives.SchemePrelude
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.*
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.TurgutsThesis.soundness.SchemeModFLocalSoundnessTests
import maf.test.TurgutsThesis.soundness.dd.SchemeSoundnessWithDeltaDebuggingTests
import maf.test.TurgutsThesis.soundness.dd.evaluation.baseline.{DDWithAllTransformationsEval, EvaluationBaseline}
import maf.test.TurgutsThesis.soundness.dd.evaluation.profiling.EvaluateProfiling
import maf.test.TurgutsThesis.soundnessBugs.*
import maf.test.{AllBenchmarks, AllSequentialBenchmarks, RandomSequentialBenchmarks, VariousSequentialBenchmarks}

trait SchemeModFLocalSoundnessTests extends EvaluationBaseline with VariousSequentialBenchmarks:
  override def parseProgram(txt: String, benchmark: String): SchemeExp =
    val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
    val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.rename(SchemeParser.undefine(transf))

class SchemeModFLocalAdaptiveTests1 extends SchemeModFLocalSoundnessTests:
  def n = 100
  def name = s"MODF LOCAL w/ ASW -- policy A (n = $n)"
  override val bugName: String = "BeginBug"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyA(n)
      with BeginBug

class SchemeModFLocalAdaptiveTests2 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "callBug"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with CallBug

class SchemeModFLocalAdaptiveTests3 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "IfBug"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with IfBug

class SchemeModFLocalAdaptiveTests4 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"

  override val bugName: Benchmark = "letBug"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with LetBug

class SchemeModFLocalAdaptiveTests5 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "LetrecBug"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with LetrecBug

class SchemeModFLocalAdaptiveTests6 extends SchemeModFLocalSoundnessTests:
  def l = 10
  override val bugName: Benchmark = "LetstarBug"
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with LetStarBug

class SchemeModFLocalAdaptiveTests7 extends SchemeModFLocalSoundnessTests:
  def l = 10
  override val bugName: Benchmark = "LiteralValueBug"
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with LiteralValueBug

class SchemeModFLocalAdaptiveTests8 extends SchemeModFLocalSoundnessTests:
  def l = 10
  override val bugName: Benchmark = "VariableBug"
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
      with VariableBug
