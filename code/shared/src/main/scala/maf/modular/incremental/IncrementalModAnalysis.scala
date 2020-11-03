package maf.modular.incremental

import maf.core._
import maf.language.change._
import maf.language.change.CodeVersion._
import maf.modular._
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.Annotations._
import maf.util.benchmarks.Timeout

// NOTE - This implementation is not thread-safe, and does not always use the local stores of the intra-component analyses!
//        Therefore, a sequential work-list algorithm is used.
trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr] {


  /* ************************************************************************* */
  /* ***** Tracking: track which components depend on which expressions. ***** */
  /* ************************************************************************* */


  /** Keeps track of whether an incremental update is in progress or not. Also used to select the right expressions in a change-expression. */
  @mutable var version: Version = Old
  /** Keeps track of which components depend on an expression. */
  @mutable private var mapping: Map[Expr, Set[Component]] = Map().withDefaultValue(Set())

  /**
   * Register that a component is depending on a given expression in the program.
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


  /* *********************************** */
  /* ***** Dependency invalidation ***** */
  /* *********************************** */


  /** Caches the dependencies of every component. Used to find dependencies that are no longer inferred (and hence can be removed). */
  @mutable var cachedDeps: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set.empty)   // Another strategy would be not to cache, but walk through the data structures.

  @nonMonotonicUpdate
  /** Deregisters a components for a given dependency, indicating the component no longer depends on it. */
  def deregister(target: Component, dep: Dependency): Unit = deps += (dep -> (deps(dep) - target))


  /* ********************************** */
  /* ***** Component invalidation ***** */
  /* ********************************** */


  /** Keep track of the number of components that have spawned a given component (excluding possibly the component). */
  @mutable var countedSpawns: Map[Component, Int] = Map().withDefaultValue(0)
  /** Keeps track of the components spawned by a component: spawner -> spawnees. Used to determine whether a component spawns less other components. */
  @mutable var cachedSpawns: Map[Component, Set[Component]] = Map().withDefaultValue(Set.empty)

  @nonMonotonicUpdate
  /**
   * Deletes information related to a component. May cause other components to be deleted as well if they are no longer spawned.
   * @note If subclasses add extra analysis state (e.g., a global store with return values), then it is up to those subclasses to override this method and extend its functionality.
   */
  def deleteComponent(cmp: Component): Unit = if (visited(cmp)) { // Only do this if we have not yet encountered the component. Note that this is not needed to prevent looping.
    for (dep <- cachedDeps(cmp)) deregister(cmp, dep) // Remove all dependencies related to this component.
    visited = visited - cmp                           // Remove the component from the visited set.
    for (to <- cachedSpawns(cmp)) unspawn(to)         // Transitively check for components that have to be deleted.

    // Delete the caches.
    cachedDeps    -= cmp
    cachedSpawns  -= cmp
    countedSpawns -= cmp // Deleting this cache is only useful for memory optimisations as the counter for cmp will be the default value of 0.
  }

  @nonMonotonicUpdate
  /** Registers that a component is no longer spawned by another component. If components become unreachable, these components will be removed. */
  def unspawn(cmp: Component): Unit = if (visited (cmp)) { // Only do this for non-collected components to avoid counts going below zero (though with the current benchmarks they seem always to be restored to zero for some reason...).
                                                           // (Counts can go below zero if an already reclaimed component is encountered here, which is possible due to the foreach in deleteDisconnectedComponents.)
    // Update the spawn count information.
    countedSpawns += (cmp -> (countedSpawns(cmp) - 1))
    if (countedSpawns(cmp) == 0) deleteComponent(cmp) else deletionFlag = true // Delete the component if it is no longer spawned by any other component.
  }


  /* ********************************************** */
  /* ***** Find and delete unreachable cycles ***** */
  /* ********************************************** */


  /** Keeps track of whether a component was unspawned but not deleted. If no such component exists, then there is no chance of having unreachable components left (all are deleted). */
  var deletionFlag: Boolean = false

  /** Computes the set of reachable components. */
  def reachableComponents(): Set[Component] = {
    var reachable: Set[Component] = Set()
    var work: Set[Component] = Set(initialComponent)
    while (work.nonEmpty) {
      val head = work.head
      work = work.tail
      if (!reachable(head)) {
        reachable += head
        work = work ++ cachedSpawns(head)
      }
    }
    reachable
  }

  /** Computes the set of unreachable components. */
  def unreachableComponents(): Set[Component] = visited -- reachableComponents()

  @nonMonotonicUpdate
  /** Deletes components that are no longer 'reachable' from the Main component given a spawning relation. */
  def deleteDisconnectedComponents(): Unit = {
    // Only perform the next steps if there was a component that was unspawned but not collected. In the other case, there can be no unreachable components left.
    //if (deletionFlag) { // Now checked in refineComponents().
      unreachableComponents().foreach(deleteComponent) // Make sure the components are actually deleted.
      deletionFlag = false
    //}
  }


  /* ************************************************************************* */
  /* ***** Incremental update: actually perform the incremental analysis ***** */
  /* ************************************************************************* */


  var regainPrecision: Boolean = true // Used for testing purposes.

  /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
  def updateAnalysis(timeout: Timeout.T, regain: Boolean = true): Unit = {
    regainPrecision = regain                                          // Used for testing pursposes.
    version = New                                                     // Make sure the new program version is analysed upon reanalysis (i.e. 'apply' the changes).
    val affected = findUpdatedExpressions(program).flatMap(mapping)
    affected.foreach(addToWorkList)
    analyze(timeout)
  }


  /* ************************************ */
  /* ***** Intra-component analysis ***** */
  /* ************************************ */


  trait IncrementalIntraAnalysis extends IntraAnalysis {

    /** Removes outdated dependencies of a component, by only keeping the dependencies that were used during the latest analysis of the component. */
    @nonMonotonicUpdate
    def refineDependencies(): Unit = {
      if (version == New) { // Only do this for an incremental update. Checking this condition is probably cheaper than performing the remaining things always (which would also be possible).
        val deltaR = cachedDeps(component) -- R  // All dependencies that were previously inferred, but are no longer inferred. This set should normally only contain elements once for every component due to monotonicity of the analysis.
        deltaR.foreach(deregister(component, _)) // Remove these dependencies. Attention: this can only be sound if the component is FULLY reanalysed!
      }
      cachedDeps += (component -> R)             // Update the cache. The cache also needs to be updated when the program is initially analysed.
    }

    /** Removes outdated components, and components that become transitively outdated, by keeping track of spawning dependencies. */
    @nonMonotonicUpdate
    def refineComponents(): Unit = {
      val Cdiff = C - component // Subtract component to avoid circular circularities due to self-recursion (this is a circularity that can easily be spotted and hence immediately omitted).

      // For each component not previously spawn by this component, increase the spawn count. Do this before removing spawns, to avoid components getting collected that have just become reachable from this component.
      (Cdiff -- cachedSpawns(component)).foreach(cmp => countedSpawns += (cmp -> (countedSpawns(cmp) + 1)))

      if (version == New) { // TODO Will anything within this if-block be executed when version == Old? No, but this might be cheaper. Should also only do something the first time a component is encountered, but that is covered without explicit check.
        val deltaC = cachedSpawns(component) -- Cdiff // The components previously spawned (except probably for the component itself), but that are no longer spawned.
        deltaC.foreach(unspawn)
        if (deletionFlag /*deltaC.nonEmpty*/) deleteDisconnectedComponents() // Delete components that are no longer reachable.
      }
      cachedSpawns += (component -> Cdiff) // Update the cache.
    }

    /**
     * First removes outdated read dependencies before performing the actual commit.
     */
    @nonMonotonicUpdate
    override def commit(): Unit = {
      if (regainPrecision) {
        refineDependencies() // First, remove excess dependencies if this is a reanalysis.
        refineComponents()   // Second, remove components that are no longer reachable (if this is a reanalysis).
      }
      super.commit()         // Then commit and trigger dependencies.
    }
  }
}
