package maf.modular.contracts

import maf.language.contracts.ScExp
import maf.modular.{FIFOWorklistAlgorithm, ModAnalysis}
import maf.util.benchmarks.Timeout

abstract class SimpleScSemantics(prg: ScExp)
    extends ModAnalysis(prg)
    with ScSmallStepSemantics
    with ScStandardComponents
    with ScPrimitives
    with FIFOWorklistAlgorithm[ScExp] {

  override def intraAnalysis(component: Component) = {
    setup
    new IntraAnalysis(component) with IntraScSmallStepSemantics
  }
}
