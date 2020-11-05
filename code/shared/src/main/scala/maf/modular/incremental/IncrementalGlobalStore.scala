package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion._
import maf.modular.{AddrDependency, Dependency, GlobalStore}
import maf.util.Annotations.{mutable, nonMonotonicUpdate}

trait IncrementalGlobalStore[Expr <: Expression] extends IncrementalModAnalysis[Expr]
                                                    with GlobalStore[Expr] { inter =>

  /** Keeps track of the provenance of values. For every address, couples every component with the value it has written to the address. */
  @mutable var provenance: Map[Addr, Map[Component, Value]] = Map().withDefaultValue(Map())
  /** Caches the addresses written by every component. Used to find addresses that are no longer written by a component. */
  @mutable var cachedWrites: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())

  /** Computes the value that should reside at a given address according to the provenance information. */
  def provenanceValue(addr: Addr): Value = provenance(addr).values.fold(lattice.bottom)(lattice.join(_, _))

  /** Delete an address if it is never written anymore. */
  def deleteAddr(addr: Addr): Unit = {
    store = store - addr
    provenance = provenance - addr
    if (deps(AddrDependency(addr)).nonEmpty) throw new Exception(s"Some components depend on a non-written address: $addr.")
    deps = deps - AddrDependency(addr)
  }

  /**
   * To be called when a write dependency is deleted. Possibly updates the store with a new value for the given address.
   * Deletes the address entirely if no component writes it anymore.
   * @param cmp  The component from which the write dependency is deleted.
   * @param addr The address corresponding to the deleted write dependency.
   */
  @nonMonotonicUpdate
  def deleteProvenance(cmp: Component, addr: Addr): Unit = {
    // Delete the provenance information corresponding to this component.
    provenance = provenance + (addr -> (provenance(addr) - cmp))
    // Compute the new value for the address and update it in the store. Remove the address if it is never written anymore.
    if (provenance(addr).isEmpty) deleteAddr(addr)
    else {
      val value: Value = provenanceValue(addr)
      if (value != inter.store(addr)) trigger(AddrDependency(addr))
      inter.store = inter.store + (addr -> value)
    }
  }

  /**
   * To be called upon a commit, with the join of the values written by the component to the given address.
   * Possibly updates the store and provenance information, and triggers dependencies if the store is updated.
   * @param cmp  The component related to the value nw written to addr.
   * @param addr The address in the store of value nw.
   * @param nw   The join of all values written by cmp to the store at addr.
   * @return Returns a boolean indicating whether the address was updated.
   */
  @nonMonotonicUpdate
  def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean = {
    val old = provenance(addr)(cmp)
    if (old == nw) return false // Nothing changed.
    // Else, there is some change. Note that both `old ⊏ nw` and `nw ⊏ old` are possible.
    provenance = provenance + (addr -> (provenance(addr) + (cmp -> nw)))
    val oldJoin = inter.store(addr) // The value currently at the given address.
    val newJoin = if (lattice.subsumes(nw, old)) lattice.join(oldJoin, nw) else provenanceValue(addr) // If `old ⊏ nw` we can just use join, which is probably more efficient.
    if (oldJoin == newJoin) return false // Even with this component writing a different value to addr, the store does not change.
    inter.store = inter.store + (addr -> newJoin)
    trigger(AddrDependency(addr))
    true
  }

  trait IncrementalGlobalStoreIntraAnalysis extends IncrementalIntraAnalysis with GlobalStoreIntra { intra =>

    /**
     *  Keep track of the values written by a component to an address.
     *  For every address, stores the join of all values written during this intra-component address.
     */
    var intraProvenance: Map[Addr, Value] = Map().withDefaultValue(lattice.bottom)

    override def writeAddr(addr: Addr, value: Value): Boolean = {
      // Update the intra-provenance: for every address, keep the join of the values written to the address.
      intraProvenance = intraProvenance + (addr -> lattice.join(intraProvenance(addr), value))
      super.writeAddr(addr, value) // Ensure the intra-store is updated so it can be used.
    }

    /**
     * Called for every written address. Returns true if the dependency needs to be triggered.
     * @note This function should be overridden to avoid the functionality of GlobalStore to be used.
     *       Even though this function could be merged into refineWrites.
     */
    override def commit(dep: Dependency): Boolean = dep match {
      case AddrDependency(addr) =>
        // There is no need to use the updateAddr function, as the store is updated by updateAddrInc. Also, this would not work, as updateAddr only performs monotonic updates.
        updateAddrInc(component, addr, intraProvenance(addr))
      case _ => super.commit(dep)
    }

    /** Refines values in the store that are no longer written to by a component. */
    @nonMonotonicUpdate
    def refineWrites(): Unit = {
      val recentWrites = intraProvenance.keySet // Writes performed during this intra-component analysis. Important: this only works when the entire component is reanalysed!
      if (version == New) {
        val deltaW = cachedWrites(component) -- recentWrites // The addresses previously written to by this component, but that are no longer written by this component.
        deltaW.foreach(deleteProvenance(component, _))
      }
      cachedWrites = cachedWrites + (component -> recentWrites)
    }

    override def commit(): Unit = {
      refineWrites() // Refine the store by removing extra addresses or using the provenance information to refine the values in the store.
      super.commit()
    }
  }

}
