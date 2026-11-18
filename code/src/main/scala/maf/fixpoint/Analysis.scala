package maf.fixpoint

import maf.domain.lattice.Lattice
import maf.syntax.Expression
import maf.util.{SmartHash, Timeout}

case class Metric(name: String, result: Double)

/**
 * Base trait of all analyses in MAF, which are modular analyses of programs of type `Expr`. It is contravariant in `Expr`: an analysis of the
 * programs of a type is also an analysis of the programs of its subtypes.
 *
 * A modular analysis is a system of constraints with side effects. It splits the analysis of a program into the analyses of components
 * (`analyzeComponent`), starting from the component of the program itself (`initialComponent`), which read (`get`) and write (`set`) parts of the
 * global analysis state, called dependencies, and discover (`spawn`) other components, through an `AnalysisState`. Its solution for a program is
 * the least fixpoint of these analyses, which a fixpoint algorithm computes (see `FixpointAlgorithm`).
 *
 * An analysis keeps no state of its own: the solutions keep it. The same analysis can therefore be solved for several programs, and with several
 * fixpoint algorithms.
 */
trait Analysis[-Expr <: Expression]:

    // parameterized by a component representation
    type Component

    /** The component of the program `prg` itself, which the analysis of `prg` starts from. */
    def initialComponent(prg: Expr): Component

    /**
     * The parts of the global analysis state (such as the results of the components), which the analyses of components read and write. Each analysis
     * defines its own dependencies, each with a lattice of values.
     */
    type Dependency <: Analysis.Dependency

    /**
     * The global analysis state, as the analysis of a component accesses it. The fixpoint algorithm provides one for every analysis of a component,
     * and decides when its changes take effect: e.g., right away, or once the analysis of the component is done.
     */
    trait AnalysisState:

        /** Spawns a discovered component. */
        def spawn(cmp: Component): Unit

        /** Returns the value of the given dependency, which the analyzed component therefore depends on. */
        def get(dep: Dependency): dep.Value

        /** Updates the given dependency with the given value. */
        def set(dep: Dependency, vlu: dep.Value): Unit

    /** Analyzes the given component, accessing the global analysis state only through the given analysis state. */
    def analyzeComponent(cmp: Component, state: AnalysisState): Unit

    /**
     * The solution of this analysis for a program, which a fixpoint algorithm computes (see `FixpointAlgorithm.solution`). It keeps the global
     * analysis state, and the state of the fixpoint algorithm.
     */
    trait Solution:

        /**
         * Computes the solution until it reaches the least fixpoint or the given timeout, whichever comes first. Another call resumes the computation
         * where this one stopped. Extra care should be taken when a timeout is given, as the solution is not guaranteed to be sound if the timeout is
         * reached: check afterwards whether the computation finished (see `finished`).
         */
        def compute(timeout: Timeout.T = Timeout.none): Unit

        /** Returns whether the computation has finished, i.e., whether the solution is the least fixpoint. */
        def finished: Boolean

        /** Returns the value of `dep` (e.g., to inspect the results of the analysis). */
        def value(dep: Dependency): dep.Value

        /** Returns the components that the analyses of the components discovered. */
        def components: Set[Component]

        /** Returns the dependencies whose values are not at the bottom of their lattice. */
        def dependencies: Set[Dependency]

        /** Returns the metrics that the computation collected (e.g., the number of analyses of components). */
        def metrics: List[Metric]

object Analysis:

    /** A dependency of any analysis: a part of the global analysis state, with a lattice of values. */
    trait Dependency extends SmartHash:
        type Value
        given lattice: Lattice[Value]
