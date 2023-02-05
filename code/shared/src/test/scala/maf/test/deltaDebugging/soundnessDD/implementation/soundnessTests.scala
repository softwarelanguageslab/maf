package maf.test.deltaDebugging.soundnessDD.implementation

import maf.core.Position
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.{SchemeModFLocal, SchemeModFLocalAnalysisResults, SchemeModFLocalNoSensitivity}
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.deltaDebugging.soundnessBugs.*
import maf.test.{CertainVariousSequentialBenchmarks, RandomSequentialBenchmarks, VariousSequentialBenchmarks}
import maf.test.deltaDebugging.soundnessDD.SoundnessDDTester

trait SchemeModFLocalSoundnessTests extends DDTester:
  override def benchmarks: Set[Benchmark] = Set("test/R5RS/various/mceval.scm")

  override def parseProgram(txt: String, benchmark: String): SchemeExp =
    val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
    val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.rename(SchemeParser.undefine(transf))

class SchemeModFLocalAdaptiveTestsA extends SchemeModFLocalSoundnessTests:
  def n = 100
  def name = s"MODF LOCAL w/ ASW -- policy A (n = $n)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with IfBug

class SchemeModFLocalAdaptiveTestsB extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with CallBug
