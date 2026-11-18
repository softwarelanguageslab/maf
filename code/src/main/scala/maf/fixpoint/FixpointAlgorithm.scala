package maf.fixpoint

import maf.analysis.Analysis
import maf.syntax.Expression
import maf.util.Timeout

/**
 * An algorithm that computes the solutions of analyses, i.e., the least fixpoints of their analyses of components. A solver that uses it keeps the
 * global analysis state, and provides the analysis states through which the analyses of the components access it: the algorithm decides when to
 * analyze which components, when the changes of an analysis take effect, and how to update a dependency with the values that are written to it.
 */
trait FixpointAlgorithm:

    /** Returns a solver of `anl` for `prg` that uses this algorithm, which has not solved anything yet (see `Solver.solve`). */
    def solver[E <: Expression](anl: Analysis[E], prg: E): Solver[anl.type]

extension [E <: Expression](anl: Analysis[E])
    /**
     * Analyzes the program `prg` with the fixpoint algorithm `alg`, until it reaches the least fixpoint or the given timeout, and returns the solver.
     * Check afterwards whether the solver finished, and call its `solve` to resume (see `Solver.solve`).
     */
    def analyze(prg: E, alg: FixpointAlgorithm, timeout: Timeout.T = Timeout.none): Solver[anl.type] =
        val solver = alg.solver(anl, prg)
        solver.solve(timeout)
        solver
