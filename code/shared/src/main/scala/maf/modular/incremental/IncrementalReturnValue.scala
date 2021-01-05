package maf.modular.incremental

import maf.core.Expression
import maf.modular._
import maf.util.Annotations.nonMonotonicUpdate

/**
  * This trait provides functionalities to handle return values in an incremental analysis.
  * @tparam Expr The type of the expressions under analysis.
  */
trait IncrementalReturnValue[Expr <: Expression]
    extends ReturnValue[Expr]
    with IncrementalGlobalStore[Expr] {

  trait IncrementalReturnValueIntraAnalysis
      extends ReturnResultIntra
      with IncrementalGlobalStoreIntraAnalysis {
    // A component always writes its return value, even when it is bottom.
    // This is needed so that reanalyses are triggered when a new component is found, and to avoid dependencies on return addresses being removed.
    // These dependencies should only be removed when the component is removed. TODO
    // intraProvenance = intraProvenance + (returnAddr(component) -> lattice.bottom)
  }
}
