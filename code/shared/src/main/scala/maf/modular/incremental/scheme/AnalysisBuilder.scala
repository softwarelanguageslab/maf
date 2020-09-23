package maf.modular.incremental.scheme

import maf.language.scheme.SchemeExp
import maf.modular._
import maf.modular.incremental.scheme.modconc._
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.modular.scheme.SchemeTypeDomain
import maf.modular.scheme.modf._
import maf.modular.scheme.ssmodconc._

object AnalysisBuilder {

  class IncrementalModConcAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                      with KKallocModConc
                                                      with IncrementalSchemeModConcSmallStepSemantics
                                                      with LIFOWorklistAlgorithm[SchemeExp]
                                                      with SchemeTypeDomain {
    val k = 1
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSmallStepIntra with KCFAIntra
  }

  class IncrementalSchemeModFAnalysis(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg)
                                                         with StandardSchemeModFComponents
                                                         with SchemeModFNoSensitivity
                                                         with SchemeModFSemantics
                                                         with LIFOWorklistAlgorithm[SchemeExp]
                                                         with SchemeTypeDomain
                                                         with IncrementalSchemeModFBigStepSemantics {
    override def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) with IncrementalSchemeModFBigStepIntra
  }
}
