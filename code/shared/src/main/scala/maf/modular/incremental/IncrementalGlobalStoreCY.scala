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

    /** The implicit flows are used for cyclic write invalidation and cover flows that are formed implicitly, i.e., through conditional branching. */
    var implicitFlows: List[Set[Addr]] = Nil

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then
            SCAs = computeSCAs()
        super.updateAnalysis(timeout)

    override def deleteComponent(cmp: Component): Unit =
        if configuration.cyclicValueInvalidation then dataFlowR = dataFlowR - cmp
        super.deleteComponent(cmp)

    override def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean = addr match {
        //case LitAddr(_) =>
        //    updateContribution(cmp, addr, nw)
        //    false // These will never update, but also must not become part of the store.
        case _ => super.updateAddrInc(cmp, addr, nw)
    }

    override def deleteContribution(cmp: Component, addr: Addr): Unit = addr match {
        //case LitAddr(_) =>
        //    provenance += (addr -> (provenance(addr) - cmp)) // TODO MAKE SURE THIS CAUSES REFINEMENT OF SCA
        //    if provenance(addr).isEmpty then provenance -= addr
        case _ => super.deleteContribution(cmp, addr)
    }

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
    var SCAs: Set[SCA] = Set()

    def computeSCAs(): Set[SCA] =
        Tarjan.scc[Addr](store.keySet, dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }))

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

    /** Refines a SCA by putting every address to its new incoming value. Computes the values to refine each address of a SCA and then performes the refinement. */
    def refineSCA(sca: SCA): Unit =
        sca.foreach { a =>
            // Computation of the new value + remove provenance and data flow that is no longer valid.
            val v = provenance(a).foldLeft(lattice.bottom) { case (acc, (c, v)) =>
              if dataFlowR(c)(a).intersect(sca).isEmpty
              then lattice.join(acc, v)
              else
                  // Delete the provenance of non-incoming values (i.e., flows within the SCA).
                  provenance += (a -> (provenance(a) - c))
                  // Mark that there is no provenance any more. REMARK check reason
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
                val dependentAddresses = SmartUnion.sunion(lattice.getAddresses(value), implicitFlows.flatten.toSet)
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
