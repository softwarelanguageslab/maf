package maf.modular.adaptive

import maf.core._
import maf.modular._
import maf.util.MonoidImplicits._

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  // update implementation for addresses and dependencies
  def updateAddr(update: Component => Component)(addr: Addr): Addr
  // requires an implementation of alpha for the abstract domain
  def updateValue(update: Component => Component)(value: Value): Value
  // updating dependencies
  override def updateDep(update: Component => Component)(dep: Dependency): Dependency = dep match {
    case AddrDependency(addr) => AddrDependency(updateAddr(update)(addr))
    case _                    => super.updateDep(update)(dep)
  }
  // when abstraction map changes, need to update the store
  override def updateAnalysisData(update: Map[Component, Component]): Unit = {
    val oldStore = store
    val oldDeps = deps
    super.updateAnalysisData(update)
    store = updateMap(updateAddr(update), updateValue(update))(store)
    oldDeps.collect { case (AddrDependency(oldAddr), oldCmps) =>
      val oldValue = updateValue(update)(oldStore.getOrElse(oldAddr, lattice.bottom))
      val newValue = store.getOrElse(updateAddr(update)(oldAddr), lattice.bottom)
      if (oldValue != newValue) {
        oldCmps.map(update).foreach(addToWorkList)
      }
    }
  }
}
