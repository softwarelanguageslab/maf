package maf.modular.taint

import maf.core.Expression
import maf.language.change.CodeVersion.*
import maf.language.scheme.SchemeExp
import maf.modular.*
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.LitAddr
import maf.modular.scheme.modf.SchemeModFComponent.Main
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion
import maf.util.graph.Tarjan

import scala.collection.immutable.*

trait GlobalStoreTaint[Expr <: Expression] extends ModAnalysis[Expr] with GlobalStore[Expr] with IncrementalAbstractDomain[Expr]:
    inter =>

    /**
     * The implicit flows cut at the module boundary, and the implicit flows created at the module boundary due to e.g., a higher-order function call.
     */
    var implicitFlowsCut: Map[Component, Set[Addr]] = Map() // Flows that decide on function calls. Needed e.g., for side-effecting functions not depending on arguments.

    /**
     * For every component, stores a map of W ~> Set[R], where the values R are the "constituents" of W.
     *
     * @note
     *   The data is NOT separated by components, it does not need to be reset upon the reanalysis of a component.
     * @note
     *   We could also store it as R ~> Set[W], but the current approach seems slightly easier (doesn't require a foreach over the set `reads`).
     */
    var dataFlowR: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set()) // changed: not separated by cmp (as it is not incremental)

    trait GlobalStoreTaintIntra extends IntraAnalysis with GlobalStoreIntra:
        intra =>

        /** Map of address dependencies W ~> Set[R]. */
        // (Temporary cache, such as the sets C, R, W.)
        var dataFlow: Map[Addr, Set[Addr]] = Map().withDefaultValue(Set())

        override def readAddr(addr: Addr): Value =
            lattice.addAddress(super.readAddr(addr), addr)

        override def writeAddr(addr: Addr, value: Value): Boolean =
            // Get the annotations and remove them so they are not written to the store. Add the implicit flows as well.
            val dependentAddresses = SmartUnion.sunion(lattice.getAddresses(value), implicitFlowsCut.getOrElse(component, Set()))
            // Store the dependencies.
            val newDependencies = SmartUnion.sunion(dataFlow(addr), dependentAddresses)
            dataFlow += (addr -> newDependencies)
            super.writeAddr(addr, lattice.removeAddresses(value))

        override def commit(): Unit =
            super.commit()
            dataFlow.foreach { case (a, as) =>
                dataFlowR += (a -> (dataFlowR(a) ++ as))
            }

    end GlobalStoreTaintIntra

    override def configString(): String = super.configString() + s"\n  with CY capabilities"
end GlobalStoreTaint
