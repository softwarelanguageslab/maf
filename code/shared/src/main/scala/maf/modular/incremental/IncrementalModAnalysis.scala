package maf.modular.incremental

import maf.core._
import maf.language.change._
import maf.language.change.CodeVersion._
import maf.modular._
import maf.util.Annotations.nonMonotonicUpdate
import maf.util.benchmarks.Timeout

// NOTE: this implementation is not thread-safe, and does not always use the local stores of the intra-component analyses!
trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] {

  /* ***** Tracking: track which components depend on which expressions. ***** */

  var version: Version = Old // Keeps track of whether an incremental update is in progress or not.
  private var mapping: Map[Expr, Set[Component]] = Map().withDefaultValue(Set()) // Keeps track of which components depend on an expression.

  /** Register that a component is depending on a given expression in the program.
   * This is needed e.g. for ModConc, where affected components cannot be determined lexically/statically.
   * This method should be called for any expression that is analysed.
   * Synchronisation should be applied when the analysis is run concurrently!
   */
  def registerComponent(expr: Expr, component: Component): Unit = mapping = mapping + (expr -> (mapping(expr) + component))

  /** Queries the program for `change` expressions and returns the expressions (within the given) that were affected by the change. */
  def findUpdatedExpressions(expr: Expr): Set[Expr] = expr match {
    case e: ChangeExp[Expr] => Set(e.old) // Assumption: change expressions are not nested.
    case e => e.subexpressions.asInstanceOf[List[Expr]].flatMap(findUpdatedExpressions).toSet
  }
  /*
  def findUpdatedExpressions(expr: Expr): Set[Expr] = {
    var work: List[Expr] = List(expr)
    var resu: List[Expr] = List()
    while (work.nonEmpty) {
      val fst :: rest = work
      work = rest
      fst match {
        case e: ChangeExp[Expr] => resu = e.old :: resu
        case e => work = SmartAppend.sappend(work, e.subexpressions.asInstanceOf[List[Expr]])
      }
    }
    resu.toSet
  }
  */

  /* ***** Regaining precision ***** */

  // Cache the dependencies of every component, to find dependencies that are no longer inferred (and hence can be removed).
  // Another strategy would be not to cache, but walk through the data structures.
  var cachedDeps: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set.empty)

  @nonMonotonicUpdate
  /** Deregisters a components for a given dependency, indicating the component no longer depends on it. */
  def deregister(target: Component, dep: Dependency): Unit = deps += (dep -> (deps(dep) - target))

  // Keep track of the components that spawned the given component.
  var componentProvenance: Map[Component, Set[Component]] = Map().withDefaultValue(Set.empty)

  override def spawn(cmp: Component, from: Component): Unit = {
    // Register that cmp is spawn from from.
    componentProvenance += (cmp -> (componentProvenance(cmp) + from))
    super.spawn(cmp, from)
  }

  // Deletes the return value from the global store if required.
  def deleteReturnAddress(cmp: Component): Unit = ()

  @nonMonotonicUpdate
  def deleteComponent(cmp: Component): Unit = {
    // Remove all dependencies related to this component.
    for (dep <- cachedDeps(cmp)) {
      deregister(cmp, dep)
    }
    // Remove the component from the visited set.
    visited = visited - cmp
    deleteReturnAddress(cmp)
    // Transitively check for components that have to be deleted.
    for (to <- componentProvenance(cmp)) {
      if (cmp != to) // A component may spawn itself (e.g., in case of recursion).
        unspawn(to, cmp)
    }
  }

  @nonMonotonicUpdate
  def unspawn(cmp: Component, from: Component): Unit = {
    componentProvenance += (cmp -> (componentProvenance(cmp) - from))
    if (componentProvenance(cmp).isEmpty) {
      // Component should be deleted.
      deleteComponent(cmp)
    }
  }

  /* ***** Incremental update: actually perform the incremental analysis ***** */

  /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
  def updateAnalysis(timeout: Timeout.T): Unit = {
    version = New // Make sure the new program version is analysed upon reanalysis (i.e. 'apply' the changes).
    val affected = findUpdatedExpressions(program).flatMap(mapping)
    affected.foreach(addToWorkList) // Affected should be cleared when there are multiple successive incremental analysis steps.
    analyze(timeout)
  }

  /* ***** Intra-component analysis ***** */

  trait IncrementalIntraAnalysis extends IntraAnalysis {

    /**
     * Removes outdated dependencies of a component, by only keeping the dependencies that were used during the latest analysis of the component.
     */
    @nonMonotonicUpdate
    def refineDependencies(): Unit = {
      if (version == New) { // Only do this for an incremental update.
        // TODO: When the program is changes, this should actually only be done once for every component if the store is not refined (since updates then continue to be monotonic).
        // TODO: However, this is a space-time trade-off probably (as we will need to store the components whose dependencies have been invalidated, this set has to be cleared upon a call to updateAnalsyis).
        val deltaR = cachedDeps(component) -- R  // All dependencies that were previously inferred, but are no longer inferred.
        deltaR.foreach(deregister(component, _)) // Remove these dependencies. Attention: this can only be sound if the component is FULLY reanalysed!
      }
      cachedDeps += (component -> R)             // Update the cache. The cache also needs to be updated when the program is initially analysed.
    }

    /**
     * First removes outdated read dependencies before performing the actual commit.
     */
    override def commit(): Unit = {
      refineDependencies()  // First, remove excess dependencies if this is a reanalysis.
      super.commit()        // Then commit and trigger dependencies.
    }
  }
}
