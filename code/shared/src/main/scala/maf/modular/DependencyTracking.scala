package maf.modular

import maf.core._

// A common, but optional extension to ModAnalysis
// Specifically, it keeps track of:
// - which components have been spawned by the last intra-analysis
// - which components have spawned which other components
trait DependencyTracking[Expr <: Expression] extends ModAnalysis[Expr] { inter =>
  var newComponents: Set[Component]                = Set()
  var dependencies: Map[Component, Set[Component]] = Map().withDefaultValue(Set.empty)
  // update some rudimentary analysis results
  //override def intraAnalysis(component: Component): DependencyTrackingIntra
  trait DependencyTrackingIntra extends IntraAnalysis {
    val visited: Set[Component] = inter.visited
    override def commit(): Unit = {
      super.commit()
      // update the bookkeeping
      newComponents = C.filterNot(visited)
      dependencies += component -> (dependencies(component) ++ C)
    }
  }
}
