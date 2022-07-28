package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.Tarjan

import scala.collection.immutable.*

trait IncrementalGlobalStoreCY[Expr <: Expression] extends IncrementalGlobalStore[Expr]:
    inter =>

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then
            val scas = computeSCAs()
            SCAs = scas.map(sca => (sca, incomingSCAValue(sca))).toMap
        super.updateAnalysis(timeout)

    override def deleteComponent(cmp: Component): Unit =
        if configuration.cyclicValueInvalidation then dataFlowR = dataFlowR - cmp
        super.deleteComponent(cmp)

    /**
     * For every component, stores a map of W ~> Set[R], where the values R are the "constituents" of W.
     *
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

    /**
     * Computes the join of all values "incoming" in this SCA. The join suffices, as the addresses in the SCA are inter-dependent (i.e., the analysis
     * will join everything together anyway).
     *
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
                if dataFlowR(component)(addr).intersect(sca).isEmpty then
                    // ...contribute to the incoming value.
                    lattice.join(value, provenance(addr)(component))
                else value
            }
        }

    /** Updates the value of all addresses in a SCA to a given value and triggers all reading and writing components. */
    def setSCAValue(sca: SCA, value: Value): Unit =
        // TODO: the new value often is bottom in some benchmarks!
        sca.flatMap(provenance).map(_._1).foreach(addToWorkList) // Add all components that wrote to the SCA to the WL.
        sca.foreach { addr =>
            store += (addr -> value)
            provenance += (addr -> provenance(addr).map(av => (av._1, value)).withDefaultValue(lattice.bottom)) // Set the provenance to the given value. `.map` removes the default value...
            trigger(AddrDependency(addr)) // Add all components that read a value from the SCA to the WL.
        }

    def resetSCA(sca: SCA): Unit =
        sca.flatMap(provenance).map(_._1).foreach(addToWorkList) // remark: are these the only ones?
        sca.foreach { addr =>
            store += (addr -> lattice.bottom)
            provenance -= addr
            trigger(AddrDependency(addr))
        }

    /** Update the SCAs. Triggers the necessary components. */
    def updateSCAs(): Unit =
        // Compute the set of new SCAs.
        val newSCAs = computeSCAs()
        // Compute a mapping from new SCAs to old SCAs.
        val map = newSCAs.map(sca => (sca, SCAs.keySet.filter(n => sca.intersect(n).nonEmpty))).toMap
        // For every new SCA, compute the incoming values.
        // TODO: we have to treat control-flow dependencies separately (i.e., not add them as part of incomingValue)
        val newSCAValues = newSCAs.map(sca => (sca, incomingSCAValue(sca))).toMap
        // For every new SCA, update the incoming values and set the SCA to the new value.
        map.foreach({ case (nw, old) =>
            val newIncoming = newSCAValues(nw)
            old.foreach { sca =>
                val oldIncoming = SCAs(sca)
                if oldIncoming != newIncoming && lattice.subsumes(oldIncoming, newIncoming) then setSCAValue(nw, newIncoming)
            }
        })
        // Update the cache (SCAs).
        SCAs = newSCAValues // TODO: why isn't this based on nw?

    //def intraSCAflow(from: Addr, to: Addr): Boolean = SCAs.keySet.exists(sca => sca.contains(from) && sca.contains(to))

    trait IncrementalGlobalStoreCYIntraAnalysis extends IncrementalGlobalStoreIntraAnalysis:
        intra =>

        /** Map of address dependencies W ~> Set[R]. */
        // (Temporary cache, such as the sets C, R, W.)
        var dataFlow: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set())

        override def readAddr(addr: Addr): Value =
            if configuration.cyclicValueInvalidation then lattice.addAddress(super.readAddr(addr), addr)
            else super.readAddr(addr)

        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation then
                // Get the annotations and remove them so they are not written to the store. Add the implicit flows as well.
                val dependentAddresses = SmartUnion.sunion(lattice.getAddresses(value), implicitFlows.flatten.toSet)
                // Store the dependencies.
                val newDependencies = SmartUnion.sunion(dataFlow(addr), dependentAddresses)
                dataFlow += (addr -> newDependencies)
                super.writeAddr(addr, lattice.removeAddresses(value))
            else super.writeAddr(addr, value)

        /*
        // remark: WRONG: not all addresses converge to the same value, so why put all addresses to the same incoming value?
        // remark: Better: put every address in the SCA to its incoming value!
        /** Refines all values in this SCA to the value "incoming". */
        def refineSCA(sca: SCA, incoming: Value): Unit =
        // Should be done for every address in the SCA because an SCC/SCA may contain "inner cycles".
            sca.foreach { addr =>
                inter.store += addr -> incoming
                //intra.store += addr -> incoming // Moved from intra-c analyssi
                provenance += (addr -> provenance(addr).map(kv => if cachedReadDeps(kv._1).contains(AddrDependency(addr)) then (kv._1, incoming) else kv))
                // Call updateAddrInc to ensure triggers happen. TODO updateAddrInc doesn't trigger => Is this the correct way of triggering?
                if updateAddrInc(component, addr, incoming) then trigger(AddrDependency(addr))
            }
         */
        override def commit(): Unit =
            super.commit()
            if configuration.cyclicValueInvalidation then
                dataFlowR += (component -> dataFlow)
                if version == New then updateSCAs()

    end IncrementalGlobalStoreCYIntraAnalysis

end IncrementalGlobalStoreCY
