package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.Tarjan

/**
 * This trait improves upon a basic incremental analysis (with dependency and component invalidation) by introducing store "provenance" tracking and
 * store lowering.
 * @tparam Expr
 *   The type of the expressions under analysis.
 */
trait IncrementalGlobalStore[Expr <: Expression] extends IncrementalModAnalysis[Expr] with GlobalStore[Expr] with IncrementalAbstractDomain[Expr]:
    inter =>

    type SCA = Set[Addr]

    /** The implicit flows are used for cyclic write invalidation and cover flows that are formed implicitly, i.e., through conditional branching. */
    var implicitFlows: List[Set[Addr]] = Nil

    /* ****************************************** */
    /* ***** Provenance tracking for values ***** */
    /* ****************************************** */

    /** Keeps track of the provenance of values. For every address, couples every component with the value it has written to the address. */
    var provenance: Map[Addr, Map[Component, Value]] = _

    /** Caches the addresses written by every component. Used to find addresses that are no longer written by a component. */
    var cachedWrites: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())

    /** Computes the value that should reside at a given address according to the provenance information. */
    // TODO: Maybe use the technique of AVL tries to get this logarithmic instead of linear? (Trick applied by IncA in another context, Szabó et al. 2018.)
    def provenanceValue(addr: Addr): Value = provenance(addr).values.fold(lattice.bottom)(lattice.join(_, _))

    /** Updates the provenance information for a specific component and address. */
    def updateProvenance(cmp: Component, addr: Addr, value: Value): Unit = provenance = provenance + (addr -> (provenance(addr) + (cmp -> value)))

    /* ******************************************** */
    /* ***** Managing cyclic value provenance ***** */
    /* ******************************************** */

    /**
     * For every component, stores a map of W ~> Set[R], where the values R are the "constituents" of W.
     * @note
     *   The data is separated by components, so it can be reset upon the reanalysis of a component.
     * @note
     *   We could also store it as R ~> Set[W], but the current approach seems slightly easier (doesn't require a foreach over the set `reads`).
     */
    var dataFlowR: Map[Component, Map[Addr, Set[Addr]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    /**
     * Keeps track of all inferred SCCs of addresses during an incremental update. To avoid confusion with analysis components, we call these Strongly
     * Connected Addresses (SCC containing addresses). For every SCA, keep track of the join of values flowing towards it from outside the SCA.
     */
    var SCAs: Map[SCA, Value] = Map()

    def computeSCAs(): Set[SCA] =
      Tarjan.scc[Addr](store.keySet, dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }))

    /*
    /**
     * Computes the join of all values "incoming" in this SCA. The join suffices, as the addresses in the SCA are inter-dependent (i.e., the analysis
     * will join everything together anyway).
     * @note
     *   This implementation computes the incoming value on a "per component" base, by using the provenance.
     * @note
     *   We do not distinguish between contributions by components that are partially incoming: if a component writes 2 values to an address and one
     *   is incoming, no incoming values will be detected. TODO Improve upon this?
     * @return
     *   The join of all values "incoming" in this SCA.
     */
    def incomingSCAValue(sca: SCA): Value =
      cachedWrites.foldLeft(lattice.bottom) { case (value, (component, addresses)) =>
        // All addresses of the SCA written by `component`...
        addresses.intersect(sca).foldLeft(value) { case (value, addr) =>
          // ...that were not influenced by an address in the SCA...
          if dataFlowR(component)(addr).union(sca).isEmpty then
              // ...contribute to the incoming value.
              lattice.join(value, provenance(addr)(component))
          else value
        }
      }

    /** Updates the value of all addresses in a SCA to a given value and triggers all reading and writing components. */
    def setSCAValue(sca: SCA, value: Value) =
        sca.flatMap(provenance).map(_._1).foreach(addToWorkList) // Add all components that wrote to the SCA to the WL.
        sca.foreach { addr =>
            store += (addr -> value)
            provenance += (addr -> provenance(addr).map(av => (av._1, value))) // Set the provenance to the given value.
            trigger(AddrDependency(addr)) // Add all ocmponents that read a value from the SCA to the WL.
        }

    /** Update the SCAs. Triggers the necessary components.
     * @param nw  The set of new SCAs.
     */
    def updateSCAs(nw: Set[SCA]): Unit =
        // Compute a mapping from old SCAs to new SCAs.
        val map = SCAs.keySet.map(sca => (sca, nw.filter(n => sca.intersect(n).nonEmpty))).toMap
        // For every new SCA, compute the incoming values.
        // TODO: we have to treat control-flow dependencies separately (i.e., not add them as part of incomingValue)
        val newSCAs = map.values.flatten.map(sca => (sca, incomingSCAValue(sca))).toMap
        // For every SCA, update the incoming values and set the SCA to the new value. // TODOL
        map.foreach({case (old, nw) =>
            val oldIncoming = SCAs(old)
            nw.foreach { sca =>
                val newIncoming = newSCAs(sca)
                if oldIncoming != newIncoming && lattice.subsumes(oldIncoming, newIncoming)
                then setSCAValue(sca, newIncoming)
            }
        })
        // Update the cache (SCAs).
        SCAs = newSCAs

    def intraSCAflow(from: Addr, to: Addr): Boolean = SCAs.keySet.find(sca => sca.contains(from) && sca.contains(to)).nonEmpty

    def computeNewSCAs(cmp: Component, flow: Map[Addr, Set[Addr]]): Set[SCA] =
        val oldFlow = dataFlowR(cmp)
        ???

     */

    /* ****************************** */
    /* ***** Write invalidation ***** */
    /* ****************************** */

    /**
     * To be called when a write dependency is deleted. Possibly updates the store with a new value for the given address. An address that is no
     * longer written will be deleted.
     * @param cmp
     *   The component from which the write dependency is deleted.
     * @param addr
     *   The address corresponding to the deleted write dependency.
     */
    def deleteProvenance(cmp: Component, addr: Addr): Unit =
        // Delete the provenance information corresponding to this component.
        provenance += (addr -> (provenance(addr) - cmp))
        // Compute the new value for the address and update it in the store.
        val value: Value = provenanceValue(addr)
        if configuration.checkAsserts then assert(lattice.subsumes(inter.store(addr), value)) // The new value can never be greater than the old value.
        if value != inter.store(addr) then
            trigger(AddrDependency(addr)) // Trigger first, as the dependencies may be removed should the address be deleted.
            // Small memory optimisation: clean up addresses entirely when they become not written anymore. This will also cause return addresses to be removed upon component deletion.
            if provenance(addr).isEmpty then deleteAddress(addr)
            else inter.store += (addr -> value)

    /**
     * Deletes an address from the store. To be used when they are no longer written by any component.
     * @note
     *   Also removes possible dependencies on this address, as well as the address's provenance!
     */
    def deleteAddress(addr: Addr): Unit =
        store -= addr // Delete the address in the actual store.
        provenance -= addr // Remove provenance information corresponding to the address (to ensure the right dependencies are triggered should the address be recreated and obtain the same value).
        deps -= AddrDependency(addr) // Given that the address is no longer in existence, dependencies on this address can be removed.

    /* ********************************** */
    /* ***** Component invalidation ***** */
    /* ********************************** */

    /**
     * Called when a component is deleted. Removes the provenance information corresponding to the addresses written by the given component, thereby
     * possibly refining the analysis store.
     * @note
     *   As a small memory optimisation, also clears `addressDependencies`. This set would be cleared anyway upon recreation and reanalysis of the
     *   deleted component `cmp`.
     * @param cmp
     *   The component that is deleted.
     */
    override def deleteComponent(cmp: Component): Unit =
        if configuration.writeInvalidation then
            cachedWrites(cmp).foreach(deleteProvenance(cmp, _))
            cachedWrites = cachedWrites - cmp
        if configuration.cyclicValueInvalidation then dataFlowR = dataFlowR - cmp
        super.deleteComponent(cmp)

    /* *************************************************** */
    /* ***** Incremental value update and refinement ***** */
    /* *************************************************** */

    /**
     * To be called upon a commit, with the join of the values written by the component to the given address. Possibly updates the store and
     * provenance information. Returns a boolean in indicating whether the store has been updated.
     * @param cmp
     *   The component that is committed.
     * @param addr
     *   The address in the store of value nw.
     * @param nw
     *   The join of all values written by cmp to the store at addr.
     * @return
     *   Returns a boolean indicating whether the address was updated, indicating whether the corresponding dependency should be triggered.
     */
    def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean =
        val old = provenance(addr)(cmp)
        if old == nw then return false // Nothing changed.
        // Else, there is some change. Note that both `old ⊏ nw` and `nw ⊏ old` - or neither - are possible.
        updateProvenance(cmp, addr, nw)
        val oldJoin = inter.store.getOrElse(addr, lattice.bottom) // The value currently at the given address.
        // If `old ⊑ nw` we can just use join, which is probably more efficient.
        val newJoin = if lattice.subsumes(nw, old) then lattice.join(oldJoin, nw) else provenanceValue(addr)
        if configuration.checkAsserts then
            assert(
              lattice.removeAddresses(newJoin) == lattice.removeAddresses(provenanceValue(addr)),
              s"$addr\n${lattice.compare(newJoin, provenanceValue(addr), "New join", "Provenance value")}"
            )
        if oldJoin == newJoin then return false // Even with this component writing a different value to addr, the store does not change.
        inter.store = inter.store + (addr -> newJoin)
        true

    /* ************************************************************************* */
    /* ***** Incremental update: actually perform the incremental analysis ***** */
    /* ************************************************************************* */

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then
            // Old method (use a local variable SCAs now since the global one is a Map now).
            val SCAs = computeSCAs()
            //incomingValues = incomingValues + SCAs.map(s => (s, incomingSCAValue(s)))
            val addrs = SCAs.flatten
            addrs.flatMap(provenance).map(_._1).foreach(addToWorkList)
            addrs.foreach { addr =>
                store = store + (addr -> lattice.bottom)
                provenance -= addr
            }
            cachedWrites = cachedWrites.map({ case (k, v) => (k, v -- addrs) }).withDefaultValue(Set())
        //SCAs = Set() // Clear the data as it is no longer needed. (This is not really required but reduces the memory footprint of the result.)
        // New method (start)
        //val scas = computeSCAs()
        //SCAs = scas.map(sca => (sca, incomingSCAValue(sca))).toMap
        super.updateAnalysis(timeout)

    /* ************************************ */
    /* ***** Intra-component analysis ***** */
    /* ************************************ */

    trait IncrementalGlobalStoreIntraAnalysis extends IncrementalIntraAnalysis with GlobalStoreIntra:
        intra =>

        /** Map of addres dependencies W ~> Set[R]. */
        // (Temporary cache, such as the sets C, R, W.
        var dataFlow: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set())

        /**
         * Keep track of the values written by a component to an address. For every address, stores the join of all values written during this
         * intra-component address.
         */
        // TODO: Perhaps collapse this data structure in the global provenance information (requires updating the information without triggers etc, but with joining).
        var intraProvenance: Map[Addr, Value] = Map().withDefaultValue(lattice.bottom)

        /* ------------------------------------ */
        /* ----- Intra-component analysis ----- */
        /* ------------------------------------ */

        ///** Called upon the (re-)analysis of a component. Here, used to clear out data structures of the incremental global store. */
        //abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit =
        //    if configuration.cyclicValueInvalidation then addressDependencies = addressDependencies - component // Avoid data becoming wrong/outdated after an incremental update.
        //    super.analyzeWithTimeout(timeout)

        /* ---------------------------------- */
        /* ----- Basic store operations ----- */
        /* ---------------------------------- */

        override def readAddr(addr: Addr): Value =
            val value = super.readAddr(addr)
            if configuration.cyclicValueInvalidation then lattice.addAddress(value, addr)
            else value

        override def writeAddr(addr: Addr, v: Value): Boolean =
            var value = v

            // CY: Update the value flow information and reset the reads information.
            if configuration.cyclicValueInvalidation then
                // Get the annotations and remove them so they are not written to the store. Add the implicit flows as well.
                val dependentAddresses = SmartUnion.sunion(lattice.getAddresses(value), implicitFlows.flatten.toSet)
                value = lattice.removeAddresses(value)
                // Store the dependencies.
                val newDependencies = SmartUnion.sunion(dataFlow(addr), dependentAddresses)
                dataFlow += (addr -> newDependencies)

            // WI: Update the intra-provenance: for every address, keep the join of the values written to the address. Do this only after possible removal of annotations.
            intraProvenance += (addr -> lattice.join(intraProvenance(addr), value))

            // Ensure the intra-store is updated so it can be used. TODO should updateAddrInc be used here (but working on the intra-store) for an improved precision?
            // Same than super.writeAddr(addr, value) except that we do not need to trigger when WI is enabled (all written addresses will be scrutinized later upon commit by doWriteIncremental).
            updateAddr(intra.store, addr, value).map { updated =>
                intra.store = updated
                if !configuration.writeInvalidation then trigger(AddrDependency(addr)) // TODO (maybe): just override `trigger`? Or just let this trigger...
            }.isDefined

        /* ------------------------------ */
        /* ----- Write invalidation ----- */
        /* ------------------------------ */

        /**
         * Registers the provenance information to the global provenance registry. Uses `updateAddrInc` to allow store refinements forthcoming from a
         * refinement of the provenance value, that were not registered due to the monotonicity of the store _during_ the intra-component analyses.
         *
         * @note
         *   Will also call updateAddrInc for addresses that were already updated by doWrite (but this is ok as the same value is used).
         * @note
         *   Incrementally updates the global store by using the contributions made to each address. Replaces doWrite, but also acts on addresses that
         *   were written but for which the store did not change. This is needed to handle strictly anti-monotonic changes.
         */
        def doWriteIncremental(): Unit = intraProvenance.foreach({ case (addr, value) =>
          if updateAddrInc(component, addr, value) then inter.trigger(AddrDependency(addr))
        })

        /** Refines values in the store that are no longer written to by a component. */
        def refineWrites(): Unit =
            // Writes performed during this intra-component analysis. Important: this only works when the entire component is reanalysed!
            val recentWrites = intraProvenance.keySet
            if version == New then
                // The addresses previously written to by this component, but that are no longer written by this component.
                val deltaW = cachedWrites(component) -- recentWrites
                deltaW.foreach(deleteProvenance(component, _))
            cachedWrites += (component -> recentWrites)

        /* ------------------------------------- */
        /* ----- Cyclic write invalidation ----- */
        /* ------------------------------------- */

        /*
        /** Refines all values in this SCA to the value "incoming". */
        def refineSCA(sca: SCA, incoming: Value): Unit =
          // Should be done for every address in the SCA because an SCC/SCA may contain "inner cycles".
          sca.foreach { addr =>
              inter.store += addr -> incoming
              intra.store += addr -> incoming
              provenance += (addr -> provenance(addr).map(kv => if cachedReadDeps(kv._1).contains(AddrDependency(addr)) then (kv._1, incoming) else kv))
              // Call updateAddrInc to ensure triggers happen. TODO updateAddrInc doesn't trigger => Is this the correct way of triggering?
              if updateAddrInc(component, addr, incoming) then trigger(AddrDependency(addr))
          }
         */

        /* ------------------ */
        /* ----- Commit ----- */
        /* ------------------ */

        /** Called for every written address. Returns true if the dependency needs to be triggered. */
        override def doWrite(dep: Dependency): Boolean = dep match
            case AddrDependency(addr) if configuration.writeInvalidation => false // Upon a commit, doWriteIncremental will take care of this. It uses updateAddrInc instead of updateAddr is used, and performs triggers accordingly.
            case _ => super.doWrite(dep)

        /** First performs the commit. Then uses information inferred during the analysis of the component to refine the store if possible. */
        override def commit(): Unit =
            // First do the super commit, which will cause the actual global store to be updated.
            // WI: Takes care of addresses that are written and caused a store update (see `doWrite`).
            super.commit()
            if configuration.writeInvalidation then
                // Refines the store by removing addresses that are no longer written or by using the provenance information to refine the values in the store.
                // WI: Takes care of addresses that are no longer written by this component.
                refineWrites()
                // Make sure all provenance values are correctly stored, even if no doWrite is triggered for the corresponding address.
                // WI: Takes care of addresses that are written and did not cause a store update.
                doWriteIncremental()
                dataFlowR += (component -> dataFlow)

    end IncrementalGlobalStoreIntraAnalysis

    /**
     * Used to initialise the analysis, so it can be used.
     * @note
     *   Scala 3 disallows non-final lazy vals to be part of a type path. As lattice is part of such type paths (e.g., lattice.L), we cannot use this
     *   value during value/class initialisation (stable type paths are required).
     */
    override def init(): Unit =
        super.init()
        provenance = Map().withDefaultValue(Map().withDefaultValue(lattice.bottom)) // Use of lattice must be delayed until after initialisation.

    override def configString(): String = super.configString() + s"\n  with an incremental global store and an $domainName"
