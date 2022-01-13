package maf.modular

import maf.core._

// A common, but optional extension to ModAnalysis
// Specifically, it keeps track of which components have spawned which other components
trait DependencyTracking[Expr <: Expression] extends ModAnalysis[Expr] { inter =>
  var dependencies: Map[Component, Set[Component]] = Map().withDefaultValue(Set.empty)
  // update some rudimentary analysis results
  override def intraAnalysis(component: Component): DependencyTrackingIntra
  trait DependencyTrackingIntra extends IntraAnalysis:
      val visited: Set[Component] = inter.visited
      override def commit(): Unit =
          super.commit()
          dependencies += component -> (dependencies(component) ++ C) // update the bookkeeping

  override def configString(): String = super.configString() + "\n  with dependency tracking"
}
