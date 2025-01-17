package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.*
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
            noStoreAddr = noStoreAddr - cmp
        super.deleteComponent(cmp)

    /* ************************************ */
    /* ************** Types *************** */
    /* ************************************ */

    type R = Addr
    type W = Addr
    type I = Addr

    /* ************************************ */
    /* ************** Caches ************** */
    /* ************************************ */

    /**
     * For every component, stores a map of W ~> Set[R], where the values R are the "constituents" of W.
     *
     * @note
     *   The data is separated by components, so it can be reset upon the reanalysis of a component.
     * @note
     *   We could also store it as R ~> Set[W], but the current approach seems slightly easier
     *   (doesn't require a foreach over the set `reads`).
     */
    var dataFlowR: Map[Component, Map[W, Set[R]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    /**
     *  Caches the implicit flows cut on the component boundary found by every component for other components.
     *  Doubly indexed to allow easy deletion upon component reanalysis.
     */
    // TODO: Given that flow nodes are now used, Set[Addr] could be replaced by Addr.
    var interComponentFlow: Map[Component, Map[Component, Set[I]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

    /** All encountered addresses that are not in the store. Used to know the nodes when invoking Tarjan. */
    var noStoreAddr: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())

    /**
     * Keeps track of all inferred SCCs of addresses during an incremental update. To avoid confusion with analysis components, we call these Strongly
     * Connected Addresses (SCC containing addresses). For every SCA, keep track of the join of values flowing towards it from outside the SCA.
     */
    var SCAs: Set[SCA] = Set()

    /* *********************************** */
    /* ************** Flows ************** */
    /* *********************************** */

    /**
     * Combines the information stored on a per-component basis in dataFlowR.
     * Computes a map of addresses written to all addresses influencing these addresses (both explicitly as well as via implicit flows).
     * @return Map[W, Set[R + I]]
     * @note Explicit flows and intra-component implicit flows are together since the implicit flows are in the monad which is not available here.
     *       Hence, the implicit intra-component flows are already added to the explicit flows upon write to the store in the semantics.
     */
    def combineDataflowR(): Map[W, Set[R]] = // dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }) // Map[W, Set[R]]
        dataFlowR.foldLeft(Map().withDefaultValue(Set[R]())) { case (res, (_, map)) => // Hopefully this is more efficient than what is commented out above.
            map.foldLeft(res) { case (res, (w, rs)) => res + (w -> SmartUnion.sunion(res(w), rs)) }
        }

    /**
     * Adds the inter-component implicit flows to the writes of all components, and adds this immediately to the explicit and intra-component implicit flows.
     * @param dataflowR The REVERSE explicit and intra-component implicit flows, without inter-component implicit flows.
     * @return The REVERSE dataflow, containing the explicit and all implicit flows.
     */
    def addInterComponentImplicitFlows(dataflowR: Map[W, Set[R]]): Map[W, Set[R]] =
        // Flows to every component (now only the flowAddrs normally).
        val combinedFlowsInterCmp: Map[Component, Set[Addr]] =
            interComponentFlow.values.foldLeft(Map[Component, Set[Addr]]()) { case (res, m) => // Use a double fold instead of flatten for efficiency?
                m.foldLeft(res) { case (res, (cmp, addr)) =>
                    res + (cmp -> SmartUnion.sunion(res.getOrElse(cmp, Set()), addr))
                }
            }
         
        // For every component that has a non-empty implicit flow context,
        // add a flow to the flow nodes of all components called by it.
        // This is a fixed-point computation as an edge may be added to a flow node of a component 
        // that has already been processed, meaning that propagation is needed.
        // TODO: It is assumed that this needs to be a fixed-point computation, but no situation has been encountered where the commented out computation below does not suffice.
        var work: Set[Component] = visited
        var addedTo: Set[Component] = Set() // Allows to still process a node when it does not have implicit flows other than ones added in this fixed-point computation.
        var processedImplicit: Set[Component] = Set() // Visited set of "expanded" nodes.
        var currDataflowR: Map[W, Set[R]] = dataflowR
        
        while work.nonEmpty
        do
            val caller = work.head
            work = work - caller
            val flowsToCaller = combinedFlowsInterCmp.getOrElse(caller, Set())
            if !processedImplicit.contains(caller) && (flowsToCaller.nonEmpty || addedTo.contains(caller))
            then
                processedImplicit = processedImplicit + caller
                currDataflowR = cachedSpawns(caller).foldLeft(currDataflowR) { case (df, callee) =>
                    val calleeAddr = FlowAddr(callee)
                    addedTo = addedTo + callee
                    work = work + callee
                    df + (calleeAddr -> (df(calleeAddr) + FlowAddr(caller)))
                }
                
        // For every component that has a non-empty implicit flow context, 
        // add a flow from the components "flow node" to all addresses it wrote.
        processedImplicit.foldLeft(currDataflowR) { (df, cmp) =>
            val cmpAddr = FlowAddr(cmp)
            cachedWrites(cmp).foldLeft(df) { case (df, w) => df + (w -> (df(w) + cmpAddr)) }
        }

        /*
        // For every component that has a non-empty implicit flow context:
        //  - Add a flow from the components "flow node" to all addresses it wrote.
        //  - Add a flow to the flow nodes of all components called by it.
        // TODO: if the combined flow is empty, nothing will be done here even though another component may have added something to the implicit flows of it.
        cachedSpawns.foldLeft(dataflowR) { case (df, (caller, callees)) =>
            val callerAddr = FlowAddr(caller)
            val flowsToCaller = combinedFlowsInterCmp.getOrElse(caller, Set())
            if flowsToCaller.nonEmpty
            then
                val written = cachedWrites(caller) // All addresses to which the implicit flows must be added (in reverse).
                val df2 = written.foldLeft(df) { case (df, w) => df + (w -> (df(w) + callerAddr))
                }
                callees.foldLeft(df2) { case (df, callee) =>
                    val calleeAddr = FlowAddr(callee)
                    df + (calleeAddr -> (df(calleeAddr) + callerAddr))
                }
            else df
        }
        */

    /* ************************************ */
    /* *************** SCAs *************** */
    /* ************************************ */

    // Keeps track of the previous flows for the incremental Tarjan.
    var previousFlowsR: Map[Addr, Set[Addr]] = Map()

    /**
     * Computes the SCAs in the program based on the explicit and implicit flows.
     */
    def computeSCAs(firstTime: Boolean = false): Set[SCA] =
        // Explicit flows + intra-component implicit flows.
        val flowsR = combineDataflowR()
        // Apply the transitive inter-component dataflow to all the writes of components and add this to the explicit flow.
        val allFlowsR = addInterComponentImplicitFlows(flowsR)
        // Then, use the expanded dataflow to compute SCAs (using the reversed flows).
        // Also take the literal addresses into account when doing so.
        if firstTime
        then
            previousFlowsR = allFlowsR
            Tarjan.scc[Addr](SmartUnion.sunion(store.keySet, noStoreAddr.values.flatten.toSet), allFlowsR)
        else
            val added = allFlowsR.map((k, v) => (k, v.diff(previousFlowsR.getOrElse(k, Set()))))
            val removed = previousFlowsR.map((k, v) => (k, v.diff(allFlowsR.getOrElse(k, Set()))))
            previousFlowsR = allFlowsR
            Tarjan.updateSCCs[Addr](SmartUnion.sunion(store.keySet, noStoreAddr.values.flatten.toSet), allFlowsR, added, removed, SCAs)

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
        }

    /**
     * Refines a SCA by putting every address to its new incoming value. Computes the values to refine each address of a SCA and then performes the
     * refinement.
     */
    def refineSCA(sca: SCA): Unit =
        //var ignored: Set[Component] = Set()
        sca.foreach {
            case _: LitAddr[_]  => // Nothing to do for literal addresses and flow addresses as they are not in the store. Contributions containing them however need to be ignored when refining other addresses.
            case _: FlowAddr[_] =>
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

    /* ************************************ */
    /* ***** Intra-component analysis ***** */
    /* ************************************ */

    trait IncrementalGlobalStoreCYIntraAnalysis extends IncrementalGlobalStoreIntraAnalysis:
        intra =>

        /** Map of address dependencies W ~> Set[R]. */
        // (Temporary cache, such as the sets C, R, W.)
        var dataFlowRIntra: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set())

        override def readAddr(addr: Addr): Value =
            if configuration.cyclicValueInvalidation then lattice.addAddress(super.readAddr(addr), addr)
            else super.readAddr(addr)

        override def writeAddr(addr: Addr, value: Value): Boolean =
            if configuration.cyclicValueInvalidation then
                // Get the annotations and remove them so they are not written to the store. Add the implicit flows as well.
                val dependentAddresses = lattice.getAddresses(value)
                // Store the dependencies.
                val newDependencies = SmartUnion.sunion(dataFlowRIntra(addr), dependentAddresses)
                dataFlowRIntra += (addr -> newDependencies)
                super.writeAddr(addr, lattice.removeAddresses(value))
            else super.writeAddr(addr, value)

        override def commit(): Unit =
            val oldStore = inter.store // Store before the analysis of this component.
            val oldDataFlowR = dataFlowR // Data flow information before the analysis of this component.
            super.commit()
            // Todo: is CY more efficient before or after WI? Or should it work at the same time?
            if configuration.cyclicValueInvalidation then
                dataFlowR += (component -> dataFlowRIntra)
                if version == New then updateSCAs(oldStore, oldDataFlowR)

    end IncrementalGlobalStoreCYIntraAnalysis

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end IncrementalGlobalStoreCY
