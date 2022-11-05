package maf.test.TurgutsThesis.soundness

import maf.core.Position
import maf.language.scheme.primitives.SchemePrelude
import maf.language.scheme.{SchemeExp, SchemeMutableVarBoxer, SchemeParser}
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.scheme.modflocal.*
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.test.TurgutsThesis.soundness.{SchemeModFLocalSoundnessTests, SchemeSoundnessWithDeltaDebuggingTests}
import maf.test.VariousSequentialBenchmarks

trait SchemeModFLocalSoundnessTests extends SchemeSoundnessWithDeltaDebuggingTests:
  override def benchmarks = Set("/Users/turgut/Desktop/cs5/thesis/AnalysisDevTools/test/R5RS/various/regex.scm")
  override def parseProgram(txt: String, benchmark: String): SchemeExp =
    val parsed = SchemeParser.parse(txt, Position.withSourcePath(benchmark))
    val prelud = SchemePrelude.addPrelude(parsed, incl = Set("__toplevel_cons", "__toplevel_cdr", "__toplevel_set-cdr!"))
    val transf = SchemeMutableVarBoxer.transform(prelud)
    SchemeParser.undefine(transf)

class SchemeModFLocalAdaptiveTestsA extends SchemeModFLocalSoundnessTests:
  def n = 100
  def name = s"MODF LOCAL w/ ASW -- policy A (n = $n)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyA(n)
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/R5RS/various/SICP-compiler.scm", // TIMES OUT
      "test/R5RS/various/mceval.scm", // TIMES OUT
      // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
      "test/R5RS/various/infinite-1.scm",
      "test/R5RS/various/infinite-2.scm",
      "test/R5RS/various/infinite-3.scm",
    ).contains(b)

class SchemeModFLocalAdaptiveTestsB extends SchemeModFLocalSoundnessTests:
  def l = 10
  def name = s"MODF LOCAL w/ ASW -- policy B (l = $l)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
      with SchemeModFLocalAdaptiveWideningPolicyB(l)
  override def isSlow(b: Benchmark): Boolean =
    Set(
      "test/R5RS/various/SICP-compiler.scm", // TIMES OUT
      "test/R5RS/various/mceval.scm", // TIMES OUT
      // these work fine in the analysis, but time out in the concrete interpreter for obvious reasons
      "test/R5RS/various/infinite-1.scm",
      "test/R5RS/various/infinite-2.scm",
      "test/R5RS/various/infinite-3.scm",
    ).contains(b)
