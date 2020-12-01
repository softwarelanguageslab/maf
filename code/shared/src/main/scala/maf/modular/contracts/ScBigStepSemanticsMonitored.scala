package maf.modular.contracts
import maf.core.Identity
import maf.util.benchmarks.Timeout
import maf.language.contracts._

/**
  * A trait that can be mixed in to provide some metrics
  * and measurements about the analysis.
  */
trait ScBigStepSemanticsMonitored extends ScBigStepSemantics {

  import ScEvalM._

  /**
    * Keeps track of how many contracts where checked
    */
  var contractApplications: Int = 0

  /**
    * Keeps track of the number of components
    * that where analysed.
    */
  var analysedComponents: Int = 0

  /**
    * This flag keeps track whether the program under analysis
    * has indicated that all its functions and function calls should be safe
    */
  var allSafe: Boolean = false

  override def intraAnalysis(component: Component): IntraScBigStepSemanticsMonitored
  trait IntraScBigStepSemanticsMonitored extends IntraScBigStepSemantics {
    override def analyze(_ignored_timeout: Timeout.T): Unit = {
      analysedComponents += 1
      super.analyze(_ignored_timeout)
    }

    override def eval(expr: ScExp): ScEvalM.ScEvalM[PostValue] = expr match {
      case ScFunctionAp(ScIdentifier("safe", _), List(), _, _) =>
        allSafe = true
        if (summary.blames.nonEmpty) {
          println("Warning:")
          println("Got non-empty blames map, program is not validated as safe!")
        }
        pure(value(lattice.injectNil))

      case ScFunctionAp(_, args, _, annotation) if annotation == Some("@unchecked") =>
        super
          .eval(expr)
          .map(res => {
            args.map(_.idn).foreach(i => removeBlame(component, i))
            res
          })

      case _ => super.eval(expr)
    }

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
