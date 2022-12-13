package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.LitAddr
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.Tarjan

import scala.collection.immutable.*

trait IncrementalGlobalStoreCY[Expr <: Expression] extends IncrementalGlobalStore[Expr]:
    inter =>

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then SCAs = computeSCAs()
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
     *  Caches the implicit flows cut on the component boundary found by every component for other components.
     *  Doubly indexed to allow easy deletion upon component reanalysis.
     */
    var interComponentFlow: Map[Component, Map[Component, Set[Addr]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    /**
     * Keeps track of all inferred SCCs of addresses during an incremental update. To avoid confusion with analysis components, we call these Strongly
     * Connected Addresses (SCC containing addresses). For every SCA, keep track of the join of values flowing towards it from outside the SCA.
     */
    var SCAs: Set[SCA] = Set()

    def computeSCAs(): Set[SCA] =
        // Explicit flows + non-cut implicit flows.
        val flowsR = dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }) // Set[W, Set[R]]
        // Compute the transitive inter-component flows. Propagate the cut implicit flows along calls. (The implicit flows to a component must also be added to all components called from it.)
        val calls = cachedSpawns
        var transitiveInterComponentFlows: Map[Component, Set[Addr]] = interComponentFlow.values.flatten.foldLeft(Map[Component, Set[Addr]]()) { case (map, (cmp, addr)) => map + (cmp -> (map(cmp) ++ addr))}
        var work = transitiveInterComponentFlows.keySet
        while work.nonEmpty
        do
            val head = work.head
            work = work - head
            val impl = transitiveInterComponentFlows.getOrElse(head, Set())
            calls.getOrElse(head, Set()).foreach { call =>
                val trans = transitiveInterComponentFlows.getOrElse(call, Set())
                val newTrans = trans ++ impl
                if trans != newTrans
                then
                    work = work + call
                    transitiveInterComponentFlows = transitiveInterComponentFlows + (call -> newTrans)
            }
        // Add the transitive cut dataflow. TODO make this a traversal with transitive addition.
        val allFlowsR = transitiveInterComponentFlows.foldLeft(flowsR) { case (df, (cmp, iFlow)) =>
            val written = cachedWrites(cmp) // All addresses to which the implicit flows must be added (in reverse).
            written.foldLeft(df) { case (df, w) => df + (w -> (df(w) ++ iFlow)) }
        }
        // Then, use the expanded dataflow to compute SCAs (using the reversed flows).
        Tarjan.scc[Addr](store.keySet, allFlowsR)

    /** Checks whether a SCA needs to be refined. */
    def refiningNeeded(sca: SCA, oldStore: Map[Addr, Value], oldDataFlowR: Map[Component, Map[Addr, Set[Addr]]]): Boolean =
        var flowsR = Map[Addr, Set[Addr]]().withDefaultValue(Set()) // Map[Writes, Set[Reads]]
        dataFlowR.foreach { case (_, wr) =>
            wr.filter(tuple => sca.contains(tuple._1)).foreach { case (write, reads) =>
                flowsR = flowsR + (write -> (flowsR(write) ++ reads))
            }
        }
        var oldFlowsR = Map[Addr, Set[Addr]]().withDefaultValue(Set())
        oldDataFlowR.foreach { case (_, wr) =>
            wr.filter(tuple => sca.contains(tuple._1)).foreach { case (write, reads) =>
                oldFlowsR = oldFlowsR + (write -> (oldFlowsR(write) ++ reads))
            }
        }
        oldFlowsR.exists { case (w, rs) =>
            rs.diff(flowsR(w)).nonEmpty || !lattice.subsumes(store(w), oldStore(w))
        }

    /**
     * Refines a SCA by putting every address to its new incoming value. Computes the values to refine each address of a SCA and then performes the
     * refinement.
     */
    def refineSCA(sca: SCA): Unit =
        sca.foreach { a =>
            // Computation of the new value + remove provenance and data flow that is no longer valid.
            val v = provenance(a).foldLeft(lattice.bottom) { case (acc, (c, v)) =>
                if dataFlowR(c)(a).intersect(sca).isEmpty then lattice.join(acc, v)
                else
                    // Delete the provenance of non-incoming values (i.e., flows within the SCA).
                    provenance += (a -> (provenance(a) - c))
                    // Mark that there is no provenance any more. (otherwise this gives key not found errors in deleteContribution/store; could be added in deleteComponent as well but makes more sense here?)
                    // REMARK: check reason + impact
                    cachedWrites = cachedWrites.map(kv => (kv._1, kv._2 - a)).withDefaultValue(Set())
                    // TODO Should we delete dataflowR as well? (Maybe this is better to avoid spurious analyses and computations as the value is deleted anyway.)
                    dataFlowR = dataFlowR.map(cm => (cm._1, cm._2 + (a -> cm._2(a).diff(sca))))
                    acc
            }
            // Refine the store. TODO remove when v is bottom!
            store += (a -> v)
            // TODO: should we trigger the address dependency here? Probably yes, but then a stratified worklist is needed for performance
            // todo: to avoid already reanalysing dependent components that do not contribute to the SCA.
            trigger(AddrDependency(a))
        }

    def updateSCAs(oldStore: Map[Addr, Value], oldDataFlowR: Map[Component, Map[Addr, Set[Addr]]]): Unit =
        SCAs = computeSCAs()
        SCAs.foreach { sca => if refiningNeeded(sca, oldStore, oldDataFlowR) then refineSCA(sca) }

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
                val dependentAddresses = lattice.getAddresses(value) // TODO: add inter-component implicit data flow (cut)
                // Store the dependencies.
                val newDependencies = SmartUnion.sunion(dataFlow(addr), dependentAddresses)
                dataFlow += (addr -> newDependencies)
                super.writeAddr(addr, lattice.removeAddresses(value))
            else super.writeAddr(addr, value)

        override def commit(): Unit =
            val oldStore = inter.store
            val oldDataFlowR = dataFlowR
            super.commit()
            if configuration.cyclicValueInvalidation then
                dataFlowR += (component -> dataFlow)
                if version == New then updateSCAs(oldStore, oldDataFlowR)

    end IncrementalGlobalStoreCYIntraAnalysis

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end IncrementalGlobalStoreCY
