package scalaam.modular.contracts

import scalaam.language.contracts.ScExp
import scalaam.modular.{FIFOWorklistAlgorithm, ModAnalysis}

abstract class SimpleScSemantics(prg: ScExp)
    extends ModAnalysis(prg)
    with ScSmallStepSemantics
    with ScStandardComponents
    with FIFOWorklistAlgorithm[ScExp] {

  override def intraAnalysis(component: Component) =
    new IntraAnalysis(component) with IntraScSmallStepSemantics
}
