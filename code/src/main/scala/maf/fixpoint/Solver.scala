package maf.fixpoint

import maf.analysis.Analysis
import maf.util.Timeout

case class Metric(name: String, result: Double)

/**
 * A solver of the analysis `analysis` for a program, which computes its solution with a fixpoint algorithm (see `FixpointAlgorithm.solver`). It keeps
 * the global analysis state, and the state of the fixpoint algorithm. The solvers of an analysis `anl` are the `Solver[anl.type]`s.
 *
 * (It is not a member of `Analysis`, so that a fixpoint algorithm can implement it with a class, and so that analyses do not depend on solvers.)
 */
trait Solver[A <: Analysis[?]]:

    /** The analysis that this solver solves. */
    val analysis: A

    /**
     * Solves the analysis until it reaches the least fixpoint or the given timeout, whichever comes first. Another call resumes where this one stopped.
     * Extra care should be taken when a timeout is given, as the values of the dependencies are not guaranteed to be sound if the timeout is reached:
     * check afterwards whether the solver finished (see `finished`).
     */
    def solve(timeout: Timeout.T = Timeout.none): Unit

    /** Returns whether the solver has finished, i.e., whether the values of the dependencies are the least fixpoint. */
    def finished: Boolean

    /** Returns the current value of `dep` (e.g., to inspect the results of the analysis). */
    def value(dep: analysis.Dependency): dep.Value

    /** Returns the components that the analyses of the components discovered so far. */
    def components: Set[analysis.Component]

    /** Returns the dependencies whose values are not at the bottom of their lattice. */
    def dependencies: Set[analysis.Dependency]

    /** Returns the metrics that the solver collected (e.g., the number of analyses of components). */
    def metrics: List[Metric]
