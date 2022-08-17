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
            val scas = computeSCAs()
            SCAs = scas.map(computeIncoming)
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
    var SCAs: Set[Map[Addr, Set[(Value, Addr)]]] = Set()

    def computeSCAs(): Set[SCA] =
        Tarjan.scc[Addr](store.keySet, dataFlowR.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }))

    /** Computes the "incoming" values for all addresses in the SCA. */
    def computeIncoming(sca: SCA): Map[Addr, Set[(Value, Addr)]] =
        // TODO: we have to treat control-flow dependencies separately (i.e., not add them as part of incomingValue) => is this already ok?
        sca.foldLeft(sca.map(_ -> Set()).toMap: Map[Addr, Set[(Value, Addr)]]) { case (map, addr): (Map[Addr, Set[(Value, Addr)]], Addr) =>
            // REMARK: here provenance does not seem to contain Lit Addresses? (wel bij het printen). Waarom worden deze dan niet in de map gestopt?
            // REMARK: wanneer een lit addr niet meer gelezen wordt, verdwijnt het uit de provenance... (maar moet er normaal origineel wel in te zien zijn)...
            println(addr.toString  + " " + provenance(addr))
            // TODO: use dataFlowR here to also loop over literal addresses that are not in the provenance (but they should?)
            dataFlowR.foldLeft(map) { case (map, (cmp, mp)): (Map[Addr, Set[(Value, Addr)]], (Component, Map[Addr, Set[Addr]])) =>
              if mp(addr).intersect(sca).isEmpty
              then
                  mp(addr).foldLeft(map) { case (map, a): (Map[Addr, Set[(Value, Addr)]], Addr) =>
                      val bnd: Set[(Value, Addr)] = map(addr)
                      val v: Value = store(a)
                      map + (addr -> (bnd + ((v, a))))
                  }
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
                  // TODO Should we delete dataflowR as well? (Maybe this is better to avoid spurious analyses and computations as the value is deleted anyway.)
                  dataFlowR = dataFlowR.map(cm => (cm._1, cm._2 + (a -> cm._2(a).diff(sca))))
                  acc
            }
            // Refine the store.
            store += (a -> v)
            // TODO: should we trigger the address dependency here? Probably yes, but then a stratified worklist is needed for performance
            // todo: to avoid already reanalysing dependent components that do not contribute to the SCA.
            trigger(AddrDependency(a))
        }

    def updateSCAs(): Unit =
        // Compute the set of new SCAs.
        val newSCAs = computeSCAs()
        // For every new SCA, compute the incoming values.
        val incoming = newSCAs.map(computeIncoming)
        println(incoming)
        // Compute a mapping from new SCAs to old SCAs.
        val tupled = incoming.map(sca => (sca, SCAs.filter(n => sca.keySet.intersect(n.keySet).nonEmpty).flatten))
        tupled.foreach({ case (nw, old) =>
          //println(old.toString() ++ " => " ++ nw.toString())
          val mp: Map[Addr, (Value, Set[Addr])] = old.toMap
          if refiningNeeded(mp, nw)
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
                if version == New then updateSCAs()

    end IncrementalGlobalStoreCYIntraAnalysis

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end IncrementalGlobalStoreCY
