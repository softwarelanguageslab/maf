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
        /*
        cachedWrites.foldLeft(Map()) {
            case (map, (component, addresses)) =>
              addresses.intersect(SCA).foldLeft(map)  // All addresses of the SCA written by `component`...
                { case (map, addr) =>
                    if dataFlowR(component)(addr).intersect(sca).isEmpty then // ...that were not influenced by an address in the SCA...
                        map += (addr -> lattice.join(map.getOrElse(addr, lattice.bottom), provenance(addr)(component)))  // ...compose incoming values.
                    else map
              }
        }
         */
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

    /*
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
    // TODO Shouldn't we look at the incoming sca value per component? As this can differ... Or at individual incoming values?
    // TODO not join!
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
     */

    trait IncrementalGlobalStoreCYIntraAnalysis extends IncrementalGlobalStoreIntraAnalysis:
        intra =>

        /** Map of address dependencies W ~> Set[R]. */
        // (Temporary cache, such as the sets C, R, W.)
        var dataFlow: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set())

        //var litEvalIntra: Set[Expr] = Set()

        //override def doWriteIncremental(): Unit = intraProvenance.foreach({
        //    case (LitAddr(_), _) => // When a literal changes, the expression changes, so this should be handled by deleteProvenance + this address must not appear in the store.
        //    case (addr, value) =>
        //        if updateAddrInc(component, addr, value) then inter.trigger(AddrDependency(addr))
        //})

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
