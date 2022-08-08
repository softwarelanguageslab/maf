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

    ///** Literals evaluated during every component's analysis. */
    //var litEval: Map[Component, Set[Expr]] = Map().withDefaultValue(Set())

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then
            val scas = computeSCAs()
            SCAs = scas.map(computeIncoming)
        super.updateAnalysis(timeout)

    override def deleteComponent(cmp: Component): Unit =
        if configuration.cyclicValueInvalidation then dataFlowR = dataFlowR - cmp
        super.deleteComponent(cmp)

    override def updateAddrInc(cmp: Component, addr: Addr, nw: Value): Boolean = addr match {
        case LitAddr(_) =>
            updateContribution(cmp, addr, nw)
            false // These will never update, but also must not become part of the store.
        case _ => super.updateAddrInc(cmp, addr, nw)
    }

    override def deleteContribution(cmp: Component, addr: Addr): Unit = addr match {
        case LitAddr(_) =>
            provenance += (addr -> (provenance(addr) - cmp)) // TODO MAKE SURE THIS CAUSES REFINEMENT OF SCA
            if provenance(addr).isEmpty then provenance -= addr
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
    var SCAs: Set[Map[Addr, (Value, Set[Addr])]] = Set()

    def computeSCAs(): Set[SCA] =
        Tarjan.scc[Addr](store.keySet, dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }))

    /** Computes the "incoming" values for all addresses in the SCA. */
    def computeIncoming(sca: SCA): Map[Addr, (Value, Set[Addr])] =
        // The same than the above but hopefully more efficient.
        // TODO: we have to treat control-flow dependencies separately (i.e., not add them as part of incomingValue) => is this already ok?
        sca.foldLeft(sca.map(_ -> (lattice.bottom, Set())).toMap) { case (map, addr) =>
            // REMARK: here provenance does not seem to contain Lit Addresses? (wel bij het printen). Waarom worden deze dan niet in de map gestopt?
            provenance(addr).foldLeft(map) { case (map, (cmp, value)) =>
                if dataFlowR(cmp)(addr).intersect(sca).isEmpty
                then
                    val (v, a) = map.getOrElse(addr, (lattice.bottom, Set()))
                    map + (addr -> (lattice.join(v, value), a ++ dataFlowR(cmp)(addr)))
                else map
            }
        }

    /** Checks whether a SCA needs to be refined. */
    def refiningNeeded(oldIncoming: Map[Addr, (Value, Set[Addr])], newIncoming: Map[Addr, (Value, Set[Addr])]): Boolean =
        oldIncoming.exists({ case (a, (v, as)) =>
            val (nv, nas) = newIncoming.getOrElse(a, (lattice.bottom, Set()))
            val c = lattice.tryCompare(v, nv)
            c.contains(1) || c.isEmpty || as.diff(nas).nonEmpty // The old value strictly subsumes the new value or both are incomparable. Or, a source is no longer present.
        })

    /** Refines a SCA by putting every address to its new incoming value. */
    def refineSCA(newIncoming: Map[Addr, (Value, Set[Addr])]): Unit =
        val sca = newIncoming.keySet
        newIncoming.foreach { case (addr, (value, _)) =>
        // TODO: update all data structures (provenance, store,...).
            store += (addr -> value)
            // Delete the provenance of non-incoming values (i.e., flows within the SCA).
            provenance += (addr -> provenance(addr).filter({ case (cmp, _) => dataFlowR(cmp)(addr).intersect(sca).isEmpty}))
            // TODO: should we trigger the address dependency here? Probably yes, but then a stratified worklist is needed for performance
            // todo: to avoid already reanalysing dependent components that do not contribute to the SCA.
            trigger(AddrDependency(addr))
        }

    def updateSCAs(): Unit =
        // Compute the set of new SCAs.
        val newSCAs = computeSCAs()
        // For every new SCA, compute the incoming values.
        val incoming = newSCAs.map(computeIncoming) // TODO: merge into refineSCA?
        // Compute a mapping from new SCAs to old SCAs.
        val tupled = incoming.map(sca => (sca, SCAs.filter(n => sca.keySet.intersect(n.keySet).nonEmpty).flatten))
        tupled.foreach({ case (nw, old) =>
          //println(old.toString() ++ " => " ++ nw.toString())
          if refiningNeeded(old.toMap, nw)
          then refineSCA(nw)
        })
        SCAs = incoming

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
            super.commit()
            if configuration.cyclicValueInvalidation then
                dataFlowR += (component -> dataFlow)
                //litEval += (component -> litEvalIntra) // Remark: or do we add it to the provenance and store the values separately for literals so they can be used to compute incoming values?
                if version == New then updateSCAs()

    end IncrementalGlobalStoreCYIntraAnalysis

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end IncrementalGlobalStoreCY
