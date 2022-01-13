package maf.modular.incremental

import maf.core._
import maf.language.change.CodeVersion._
import maf.language.change._
import maf.modular._
import maf.modular.worklist._
import maf.util.benchmarks.Timeout
import maf.util.datastructures.SmartUnion.sunion

/**
 * This trait provides the implementation of an incremental modular analysis. Upon a change, schedules the directly affected components for analysis
 * and initiates the reanalysis. Apart from that, several optimisations are implemented that can be optionally enabled by means of an
 * `IncrementalConfiguration`.
 * @note
 *   The incremental analysis is not thread-safe and does not always use the local stores of the intra-component analyses! Therefore, a sequential
 *   work-list algorithm is required.
 * @note
 *   No successive changes to the program are supported by the implementation. However, this "issue" can easily be resolved.
 */
trait IncrementalModAnalysis[Expr <: Expression] extends ModAnalysis[Expr] with SequentialWorklistAlgorithm[Expr]:

    /**
     * The incremental configuration for this analysis. This configuration keeps track of the optimisations that are used.
     * @note
     *   The incremental configuration can be swapped. However, care must be taken when doing so: not all changes may produce the intended result as
     *   the required caches are only maintained when the optimisation is enabled.
     */
    var configuration: IncrementalConfiguration

    /* ************************************************************************ */
    /* ***** Tracking: track which components depend on which expressions ***** */
    /* ************************************************************************ */

    /** Keeps track of whether an incremental update is in progress or not. Also used to select the right expressions in a change-expression. */
    var version: Version = Old

    /** Keeps track of which components depend on an expression. */
    var mapping: Map[Expr, Set[Component]] = Map().withDefaultValue(Set()) // TODO: when a new program version causes changes, the sets may need to shrink again (~ cached dependencies etc).

    /**
     * Register that a component is depending on a given expression in the program. This is needed e.g. for process-modular analyses, where affected
     * components cannot be determined lexically. This method should be called for any expression that is analysed.
     */
    def registerComponent(expr: Expr, component: Component): Unit = mapping = mapping + (expr -> (mapping(expr) + component))

    /** Queries the given expression for `change` expressions and returns the expressions (within the given one) that were affected by the change. */
    def findUpdatedExpressions(expr: Expr): Set[Expr] = expr match
        case e: ChangeExp[Expr] => Set(e.old) // Assumption: change expressions are not nested.
        case e                  => e.subexpressions.asInstanceOf[List[Expr]].flatMap(findUpdatedExpressions).toSet

    /* *********************************** */
    /* ***** Dependency invalidation ***** */
    /* *********************************** */

    /**
     * Caches the read dependencies of every component. Used to find dependencies that are no longer inferred (and hence can be removed).
     * @note
     *   Another strategy would be not to cache, but to walk through the data structures of the analysis (e.g., deps).
     * @note
     *   This cache is also required for component invalidation!
     */
    var cachedReadDeps: Map[Component, Set[Dependency]] = Map().withDefaultValue(Set.empty)

    /** Deregisters a components for a given dependency, indicating the component no longer depends on it. */
    def deregister(target: Component, dep: Dependency): Unit = deps += (dep -> (deps(dep) - target))

    /* ********************************** */
    /* ***** Component invalidation ***** */
    /* ********************************** */

    /**
     * Keeps track of the components spawned by a component (spawner -> spawnees). Used to determine whether a component spawns less other components.
     */
    var cachedSpawns: Map[Component, Set[Component]] = Map().withDefaultValue(Set.empty)

    /**
     * Deletes information related to a component. May cause other components to be deleted as well if they are no longer spawned.
     *
     * @note
     *   If subclasses add extra analysis state (e.g., a global store with return values), then it is up to those subclasses to override this method
     *   and extend its functionality to remove the added state related to the deleted component (e.g., to remove the return value from the store).
     */
    def deleteComponent(cmp: Component): Unit =
        for dep <- cachedReadDeps(cmp) do deregister(cmp, dep) // Remove all dependencies related to this component.
        visited = visited - cmp // Remove the component from the visited set.
        // Remove the component from the work list, as it may be present there, to avoid it being analysed if it has been scheduled before.
        workList = workList - cmp

        // Delete the caches.
        cachedReadDeps -= cmp
        cachedSpawns -= cmp

    /** Computes the set of reachable components (tracing from the Main component). */
    def reachableComponents(): Set[Component] =
        var reachable: Set[Component] = Set()
        var work: Set[Component] = Set(initialComponent)
        while work.nonEmpty do
            val head = work.head
            work = work.tail
            if !reachable(head) then
                reachable += head
                work = sunion(work, cachedSpawns(head)) // Perform a "smart union".
        reachable

    /** Computes the set of unreachable components. */
    def unreachableComponents(): Set[Component] = visited -- reachableComponents()

    /* ************************************************************************* */
    /* ***** Incremental update: actually perform the incremental analysis ***** */
    /* ************************************************************************* */

    /** Perform an incremental analysis of the updated program, starting from the previously obtained results. */
    def updateAnalysis(timeout: Timeout.T): Unit =
        version = New // Make sure the new program version is analysed upon reanalysis (i.e., 'apply' the changes).
        val affected = findUpdatedExpressions(program).flatMap(mapping)
        affected.foreach(addToWorkList)
        analyzeWithTimeout(timeout)

    /* ************************************ */
    /* ***** Intra-component analysis ***** */
    /* ************************************ */

    trait IncrementalIntraAnalysis extends IntraAnalysis:

        /* ----------------------------------- */
        /* ----- Dependency invalidation ----- */
        /* ----------------------------------- */

        /**
         * Removes outdated dependencies of a component, by only keeping the dependencies that were used during the latest analysis of the component.
         */
        def refineDependencies(): Unit =
          if version == New then // Check for efficiency but can be omitted.
              // All dependencies that were previously inferred, but are no longer inferred. This set should normally only contain elements once for every component due to monotonicity of the analysis.
              val deltaR = cachedReadDeps(component) -- R
              deltaR.foreach(deregister(component, _)) // Remove these dependencies. Attention: this can only be sound if the component is FULLY reanalysed!

        /* ---------------------------------- */
        /* ----- Component invalidation ----- */
        /* ---------------------------------- */

        /** Removes outdated components, and components that become transitively outdated, by keeping track of spawning dependencies. */
        def refineComponents(): Unit =
            val old = cachedSpawns(component)
            cachedSpawns += (component -> C) // Update the cache.
            if version == New && (old -- C).nonEmpty then // Check whether tracing is needed: only if a component that was previously spawned is no longer spawned by this component. We do this to avoid spurious computations.
                unreachableComponents().foreach(deleteComponent) // Delete components that are no longer 'reachable' from the Main component given a spawning relation. Important: use the updated cache!

        /* ------------------ */
        /* ----- Commit ----- */
        /* ------------------ */

        /** First removes outdated read dependencies and components before performing the actual commit. */
        override def commit(): Unit =
            if configuration.dependencyInvalidation then refineDependencies() // First, remove excess dependencies if this is a reanalysis.
            if configuration.componentInvalidation then refineComponents() // Second, remove components that are no longer reachable (if this is a reanalysis).
            if configuration.componentInvalidation || configuration.dependencyInvalidation then
                // Update the cache. The cache also needs to be updated when the program is initially analysed.
                // This is also needed for CI, as otherwise components can come "back to life" as dependencies corresponding to deleted components cannot be removed (easily).
                // Note that this has to be done _after_ dependency invalidation!
                cachedReadDeps += (component -> R)
            super.commit() // Then commit and trigger dependencies.

    end IncrementalIntraAnalysis

    override def configString(): String =
      super.configString() + s"\n  with incremental capabilities\n  using the following configuration: $configuration"
