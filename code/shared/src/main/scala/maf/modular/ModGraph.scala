package maf.modular

import maf.language.scheme.*

/**
 * Keeps track of the triggered, read (from ModAnalysis) and call dependencies.
 *
 * They can be used in an analysis to do a particular set of operations, such as finding paths in the ModF graph, or computing an SCC.
 */
trait ModGraph[E <: SchemeExp] extends ModAnalysis[E]:
    var triggeredDeps: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set())
    var calledDeps: Map[Component, Set[Component]] = Map().withDefaultValue(Set())

    override def intraAnalysis(component: Component): IntraTrackAnalysis

    trait IntraTrackAnalysis extends IntraAnalysis:
        override def trigger(dep: Dependency): Unit =
            triggeredDeps = triggeredDeps + (component -> (triggeredDeps(component) + dep))
            super.trigger(dep)

        override def spawn(calledCmp: Component): Unit =
            calledDeps = calledDeps + (component -> (calledDeps(component) + calledCmp))
            super.spawn(calledCmp)
