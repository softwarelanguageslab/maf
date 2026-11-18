package maf.fixpoint

import maf.analysis.Analysis
import maf.syntax.Expression

/**
 * An algorithm that computes the solutions of analyses, i.e., the least fixpoints of their analyses of components. A solver that uses it keeps the
 * global analysis state, and provides the analysis states through which the analyses of the components access it: the algorithm decides when to
 * analyze which components, when the changes of an analysis take effect, and how to update a dependency with the values that are written to it.
 */
trait FixpointAlgorithm:

    /** Returns a solver of `anl` for `prg` that uses this algorithm, which has not solved anything yet (see `Solver.solve`). */
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver
