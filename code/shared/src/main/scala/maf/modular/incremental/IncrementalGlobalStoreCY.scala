package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.LitAddr
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.util.ColouredFormatting.*
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
        if configuration.cyclicValueInvalidation
        then
            dataFlowR = dataFlowR - cmp
            litAddr = litAddr - cmp
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

    /** All encountered literal addresses. */
    var litAddr: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())

    /**
     * Keeps track of all inferred SCCs of addresses during an incremental update. To avoid confusion with analysis components, we call these Strongly
     * Connected Addresses (SCC containing addresses). For every SCA, keep track of the join of values flowing towards it from outside the SCA.
     */
    var SCAs: Set[SCA] = Set()

    /**
     * Computes a map of addresses written to all addresses influencing these addresses via explicit data flow or via intra-component implicit flows.
     * @return Map[W, Set[R + I]]
     * @note These are together, since the implicit flows are in the monad which is not available here. Hence, the implicit intra-component flows are
     *       already added to the explicit flows upon write to the store in the semantics.
     */
    def explicitAndIntraComponentImplicitFlowsR(): Map[Addr, Set[Addr]] = // dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }) // Map[W, Set[R]]
        dataFlowR.foldLeft(Map().withDefaultValue(Set[Addr]())) { case (res, (_, map)) => // Hopefully this is more efficient than what is commented out above.
            map.foldLeft(res) { case (res, (w, rs)) => res + (w -> SmartUnion.sunion(res(w), rs)) }
        }

    /**
     * Applies the inter-component implicit flows transitively. When a component A has implicit flows a* attached and calls a component B, then
     * the implicit flows a* must also be attached to B. After all, if an implicit flow causes a call of A, then this implicit flow also causes the call to B.
     *
     * @return A map containing the set of transitive implicit flows for every component.
     * @note These flows cannot be kept/computed right away by the intra-component analyses as this would created cycles again (on the level of these flows).
     */
    def computeTransitiveInterComponentFlows(): Map[Component, Set[Addr]] =
        // Compute the transitive inter-component flows. Propagate the cut implicit flows along calls. (The implicit flows to a component must also be added to all components called from it.)
        val calls = cachedSpawns
        var transitiveInterComponentFlows: Map[Component, Set[Addr]] =
            interComponentFlow.values.foldLeft(Map[Component, Set[Addr]]()) { case (res, m) => // Use a double fold instead of flatten for efficiency?
                m.foldLeft(res) { case (res, (cmp, addr)) =>
                    res + (cmp -> SmartUnion.sunion(res.getOrElse(cmp, Set()), addr))
                }
            }
        var work = transitiveInterComponentFlows.keySet
        while work.nonEmpty // Can be stratified maybe.
        do
            val head = work.head
            work = work - head
            val impl = transitiveInterComponentFlows.getOrElse(head, Set())
            calls.getOrElse(head, Set()).foreach { call =>
                val trans = transitiveInterComponentFlows.getOrElse(call, Set())
                val newTrans = SmartUnion.sunion(trans, impl)
                if trans != newTrans
                then
                    work = work + call
                    transitiveInterComponentFlows = transitiveInterComponentFlows + (call -> newTrans)
            }
        transitiveInterComponentFlows

    /**
     * Adds the transitive inter-component implicit flows to the writes of all components, and adds this immediately to the explicit and intra-component implicit flows.
     * @param flowsR The REVERSE explicit and intra-component implicit flows.
     * @param transitiveFlows The transitive inter-component implicit flows, not reversed.
     * @return The REVERSE dataflow, containing the explicit and all implicit flows.
     */
    def attachTransitiveFlowsToFlowsR(flowsR: Map[Addr, Set[Addr]], transitiveFlows: Map[Component, Set[Addr]]): Map[Addr, Set[Addr]] =
        transitiveFlows.foldLeft(flowsR) { case (df, (cmp, iFlow)) =>
            val written = cachedWrites(cmp) // All addresses to which the implicit flows must be added (in reverse).
            written.foldLeft(df) { case (df, w) => df + (w -> SmartUnion.sunion(df(w), iFlow)) }
        }

    /**
     * Computes the SCAs in the program based on the explicit and implicit flows.
     */
    def computeSCAs(): Set[SCA] =
        // Explicit flows + intra-component implicit flows.
        val flowsR = explicitAndIntraComponentImplicitFlowsR()
        // Compute the transitive inter-component flows.
        val transitiveInterComponentFlows = computeTransitiveInterComponentFlows() // TODO: there are now also implicit flows from literal addresses...
        // Apply the transitive inter-component dataflow to all the writes of components and add this to the explicit flow.
        val allFlowsR = attachTransitiveFlowsToFlowsR(flowsR, transitiveInterComponentFlows)
        // Then, use the expanded dataflow to compute SCAs (using the reversed flows).
        // Also take the literal addresses into account when doing so.
        Tarjan.scc[Addr](SmartUnion.sunion(store.keySet, litAddr.values.flatten.toSet), allFlowsR)

    /** Checks whether a SCA needs to be refined. */
    def refiningNeeded(sca: SCA, oldStore: Map[Addr, Value], oldDataFlowR: Map[Component, Map[Addr, Set[Addr]]]): Boolean =
        var flowsR = Map[Addr, Set[Addr]]().withDefaultValue(Set()) // Map[Writes, Set[Reads]]
        dataFlowR.foreach { case (_, wr) =>
            wr.filter(tuple => sca.contains(tuple._1)).foreach { case (write, reads) =>
                flowsR = flowsR + (write -> SmartUnion.sunion(flowsR(write), reads))
            }
        }
        var oldFlowsR = Map[Addr, Set[Addr]]().withDefaultValue(Set())
        oldDataFlowR.foreach { case (_, wr) =>
            wr.filter(tuple => sca.contains(tuple._1)).foreach { case (write, reads) =>
                oldFlowsR = oldFlowsR + (write -> SmartUnion.sunion(oldFlowsR(write), reads))
            }
        }
        oldFlowsR.exists { case (w, rs) =>
            // TODO: why does there also need to be a getOrElse at oldstore(w)? If it is not there, it cannot be part of the SCA?
            rs.diff(flowsR(w)).nonEmpty ||
                rs.exists { r => !lattice.subsumes(store.getOrElse(r, lattice.bottom), oldStore.getOrElse(r, lattice.bottom))}
                //!lattice.subsumes(store.getOrElse(w, lattice.bottom), oldStore.getOrElse(w, lattice.bottom)) // Shouldn't this be for RS? => This is wrong: reversed.
        }

    /**
     * Refines a SCA by putting every address to its new incoming value. Computes the values to refine each address of a SCA and then performes the
     * refinement.
     */
    def refineSCA(sca: SCA): Unit =
        //var ignored: Set[Component] = Set()
        sca.foreach {
            case _: LitAddr[_] => // Nothing to do for literal addresses as they are not in the store. Contributions containing them however need to be ignored when refining other addresses.
            case a =>
                // Computation of the new value + remove provenance and data flow that is no longer valid.
                val v = provenance(a).foldLeft(lattice.bottom) { case (acc, (c, v)) =>
                    // Todo: does the test on literal addresses not prune away too much information?? Still doesn't seem to work + heap space errors: && !dataFlowR(c)(a).exists(_.isInstanceOf[LitAddr[_]])
                    if dataFlowR(c)(a).intersect(sca).isEmpty then lattice.join(acc, v)
                    else
                        // Indicate that this component needs to be reanalysed, as at some point, its contribution is ignored.
                        //ignored += c // TODO: is this always needed or only e.g., in the case where the store value was updated (and not when a flow disappeared)?
                        // Delete the provenance of non-incoming values (i.e., flows within the SCA).
                        provenance += (a -> (provenance(a) - c))
                        // Mark that there is no provenance any more. (otherwise this gives key not found errors in deleteContribution/store; could be added in deleteComponent as well but makes more sense here?)
                        // REMARK: check reason + impact
                        cachedWrites = cachedWrites + (c -> (cachedWrites(c) - a))
                        //cachedWrites = cachedWrites.map(kv => (kv._1, kv._2 - a)).withDefaultValue(Set())
                        // TODO Should we delete dataflowR as well? (Maybe this is better to avoid spurious analyses and computations as the value is deleted anyway.)
                        dataFlowR = dataFlowR + (c -> (dataFlowR(c) + (a -> dataFlowR(c)(a).diff(sca))))
                        //dataFlowR = dataFlowR.map(cm => (cm._1, cm._2 + (a -> cm._2(a).diff(sca))))
                        acc
                }
                //val old = store.getOrElse(a, lattice.bottom)
                //if old != v then // No need for a trigger when nothing changes. TODO adding this tests causes unsoundness and errors?
                // Refine the store. TODO remove when v is bottom!
                if configuration.checkAsserts then assert(lattice.subsumes(inter.store(a), v))
                store += (a -> v)
                if configuration.checkAsserts then assert(store(a) == provenanceValue(a))
                // TODO: should we trigger the address dependency here? Probably yes, but then a stratified worklist is needed for performance
                // todo: to avoid already reanalysing dependent components that do not contribute to the SCA.
                trigger(AddrDependency(a))
        }
        // Add all these components to the worklist at once (should perform better as it avoids duplicate additions).
        //addToWorkList(ignored)

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
            val oldStore = inter.store // Store before the analysis of this component.
            val oldDataFlowR = dataFlowR // Data flow information before the analysis of this component.
            super.commit()
            // Todo: is CY more efficient before or after WI? Or should it work at the same time?
            if configuration.cyclicValueInvalidation then
                dataFlowR += (component -> dataFlow)
                if version == New then updateSCAs(oldStore, oldDataFlowR)

    end IncrementalGlobalStoreCYIntraAnalysis

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end IncrementalGlobalStoreCY
