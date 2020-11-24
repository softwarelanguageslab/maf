package maf.modular.contracts
import maf.core.Identity

trait ScBigStepSemanticsMonitored extends ScBigStepSemantics {
  var contractApplications: Int = 0

  override def intraAnalysis(component: Component): IntraScBigStepSemanticsMonitored
  trait IntraScBigStepSemanticsMonitored extends IntraScBigStepSemantics {
    override def monFlat(
        contract: (Value, PC),
        expressionValue: (Value, PC),
        blamedIdentity: Identity
    ): ScEvalM.ScEvalM[(Value, PC)] = {
      contractApplications += 1
      super.monFlat(contract, expressionValue, blamedIdentity)
    }
  }

}
