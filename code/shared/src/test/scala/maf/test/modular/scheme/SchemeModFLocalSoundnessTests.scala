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
    new SchemeModFLocal(prg)  with SchemeConstantPropagationDomain 
                              with SchemeModFLocalNoSensitivity 
                              with FIFOWorklistAlgorithm[SchemeExp]
                              with SchemeModFLocalAnalysisResults
}

class SchemeModFLocalCallSiteSensitiveSoundnessTests extends SchemeModFLocalSoundnessTests with VariousSequentialBenchmarks {
  def name = "MODF LOCAL (call-site sensitive)"
  def analysis(prg: SchemeExp) =
    new SchemeModFLocal(prg)  with SchemeConstantPropagationDomain 
                              with SchemeModFLocalCallSiteSensitivity 
                              with FIFOWorklistAlgorithm[SchemeExp]
                              with SchemeModFLocalAnalysisResults {
      val k = 1
    }
}
