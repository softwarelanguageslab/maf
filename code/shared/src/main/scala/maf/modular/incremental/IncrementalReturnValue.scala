package maf.modular.incremental

import maf.core.Expression
import maf.modular._
import maf.util.Annotations.nonMonotonicUpdate

trait IncrementalReturnValue[Expr <: Expression] extends ReturnValue[Expr] with IncrementalGlobalStore[Expr] {

  /**
   * Deletes an address from the store. To be used when deleting return values upon component deletion.
   * @note Other addresses should not be deleted this way.
   */
  def deleteAddress(addr: Addr): Unit = {
    store -= addr       // Delete the address in the actual store.
    provenance -= addr  // Remove provenance information corresponding to the address (to ensure the right dependencies are triggered should the component be recreated and have the same return value).
  }

  // COMMENTED PARTS ALREADY ENABLED IN SCHEME ANALYSES BECAUSE THIS SHOULD ALSO BE DONE WHEN INCREMENTALGLOBALSTORE IS DISABLED.
  /**
   * Deletes the return value from the global store (sets it to bottom), as well as the corresponding dependencies.
   * A return value should <i>only</i> be removed from the store if the corresponding component is removed.
   * @note This function is also needed when the trait IncrementalGlobalStore is not used.
   */
  @nonMonotonicUpdate
  override def deleteComponent(cmp: Component): Unit = { // This cannot directly go into the intra-component analysis, as the values will then again become joined when the store is committed...
    //if (log) logger.log(s"RMCM* $cmp")
    deleteAddress(returnAddr(cmp)) // TODO: Couldn't this also be deleteProvenance(cmp, returnAddr(cmp))? Or will this cause more deps to be triggered needlessly (are they arlready removed here)?
    //deps -= AddrDependency(returnAddr(cmp))
    super.deleteComponent(cmp)
  }

  trait IncrementalReturnValueIntraAnalysis extends ReturnResultIntra with IncrementalGlobalStoreIntraAnalysis {
    // A component always writes its return value, even when it is bottom.
    // This is needed so that reanalyses are triggered when a new component is found, and to avoid dependencies on return addresses being removed.
    // These dependencies should only be removed when the component is removed.
    // intraProvenance = intraProvenance + (returnAddr(component) -> lattice.bottom)
  }
}
