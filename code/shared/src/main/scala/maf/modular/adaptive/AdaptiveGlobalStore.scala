package maf.modular.adaptive

import maf.core._
import maf.modular._
import maf.util.MonoidImplicits._

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr]:
    // adapt implementation for addresses and dependencies
    def adaptAddr(addr: Addr): Addr
    // requires an implementation of adapt for the abstract domain
    def adaptValue(value: Value): Value
    // updating dependencies
    override def adaptDep(dep: Dependency): Dependency = dep match
        case AddrDependency(addr) => AddrDependency(adaptAddr(addr))
        case _                    => super.adaptDep(dep)
    // when abstraction map changes, need to update the store
    override def adaptAnalysis(): Unit =
        val oldStore = store
        val oldDeps = deps
        super.adaptAnalysis()
        // TODO: can this be done in one go?
        store = adaptMap(adaptAddr, adaptValue)(store)
        oldDeps.collect { case (AddrDependency(oldAddr), oldCmps) =>
          val oldValue = oldStore.getOrElse(oldAddr, lattice.bottom)
          val newValue = store.getOrElse(adaptAddr(oldAddr), lattice.bottom)
          if adaptValue(oldValue) != newValue then oldCmps.map(adaptComponent).foreach(addToWorkList)
        }
