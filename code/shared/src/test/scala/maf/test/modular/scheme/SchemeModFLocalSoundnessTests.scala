package maf.test.modular.scheme

import maf.language.scheme._
import maf.modular.scheme.modflocal._
import maf.test._
import maf.modular.scheme._
import maf.modular.worklist._

trait SchemeModFLocalSoundnessTests extends SchemeSoundnessTests

class SchemeModFLocalInsensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks {
  def name = "MODF LOCAL (context-insensitive)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalNoSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults
  override def isSlow(b: Benchmark): Boolean = 
    Set(
      "test/R5RS/various/rsa.scm",
      "test/R5RS/various/quasiquoting-simple.scm",
      "test/R5RS/various/SICP-compiler.scm",
      "test/R5RS/various/church.scm",
      "test/R5RS/various/foo.scm",
      "test/R5RS/various/mceval.scm",
      "test/R5RS/various/regex.scm",
      "test/R5RS/various/grid.scm",
      "test/R5RS/various/widen.scm",
      "test/R5RS/various/infinite-1.scm",
      "test/R5RS/various/infinite-2.scm",
      "test/R5RS/various/infinite-3.scm",
      "test/R5RS/various/four-in-a-row.scm"
    ).contains(b)

}

class SchemeModFLocalCallSiteSensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks {
  def name = "MODF LOCAL (call-site sensitive)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)
      with SchemeConstantPropagationDomain
      with SchemeModFLocalCallSiteSensitivity
      with FIFOWorklistAlgorithm[SchemeExp]
      with SchemeModFLocalAnalysisResults {
      val k = 1
    }
  override def isSlow(b: String): Boolean = true // don't run this for fast tests
}
