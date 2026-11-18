package maf.fixpoint

import maf.syntax.Expression

/**
 * An algorithm that computes the solutions of analyses, i.e., the least fixpoints of their analyses of components. A solution that it computes keeps
 * the global analysis state, and provides the analysis states through which the analyses of the components access it: the algorithm decides when to
 * analyze which components, when the changes of an analysis take effect, and how to update a dependency with the values that are written to it.
 */
trait FixpointAlgorithm:

    /** Returns the solution of `anl` for `prg` that this algorithm computes, of which nothing is computed yet (see `Solution.compute`). */
    def solution[E <: Expression](anl: Analysis[E], prg: E): anl.Solution
