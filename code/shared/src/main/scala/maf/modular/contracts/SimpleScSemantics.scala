package maf.modular.contracts

import maf.language.contracts.ScExp
import maf.modular.ModAnalysis
import maf.modular.worklist.FIFOWorklistAlgorithm

abstract class SimpleScSemantics(prg: ScExp)
    extends ModAnalysis(prg)
    with ScBigStepSemantics
    with ScStandardComponents
    with ScPrimitives
    with FIFOWorklistAlgorithm[ScExp] {

  override def intraAnalysis(component: Component) = {
    setup()
    new IntraAnalysis(component) with IntraScBigStepSemantics
  }
}
