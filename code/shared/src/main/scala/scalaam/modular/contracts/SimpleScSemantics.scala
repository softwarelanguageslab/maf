package scalaam.modular.contracts

import scalaam.language.contracts.ScExp
import scalaam.modular.{FIFOWorklistAlgorithm, ModAnalysis}
import scalaam.util.benchmarks.Timeout

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
