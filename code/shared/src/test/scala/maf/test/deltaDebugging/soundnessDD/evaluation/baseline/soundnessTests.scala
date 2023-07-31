package maf.test.deltaDebugging.soundnessDD.evaluation.baseline

import maf.language.scheme.primitives.SchemePrelude
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.*
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.deltaDebugging.realBugs.*
import maf.test.{CertainVariousSequentialBenchmarks, RandomSequentialBenchmarks, VariousSequentialBenchmarks}
import maf.core.Position

trait SchemeModFLocalSoundnessTests extends BaselineTester with CertainVariousSequentialBenchmarks:
  //override def benchmarks: Set[Benchmark] = Set("test/R5RS/various/SICP-compiler.scm")
  override def parseProgram(txt: String, benchmark: String): SchemeExp =
    val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
    val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.rename(SchemeParser.undefine(transf))

class SchemeModFLocalAdaptiveTests1 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "RealBug1"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with RealBug1

class SchemeModFLocalAdaptiveTests2 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "RealBug2"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with RealBug2

class SchemeModFLocalAdaptiveTests3 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "RealBug3"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with RealBug3

class SchemeModFLocalAdaptiveTests4 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "RealBug4"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with RealBug4

class SchemeModFLocalAdaptiveTests5 extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  override val bugName: Benchmark = "RealBug5"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with RealBug5

