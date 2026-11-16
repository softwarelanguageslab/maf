package maf.fixpoint

import maf.domain.lattice.Lattice
import maf.syntax.*
import maf.util.{SmartHash, Timeout}

import scala.concurrent.duration._

case class Metric(name: String, result: Double)

/**
 * Base class of all analyses in MAF, which are modular analyses. Specifies the elements (fields, methods, and types) to be provided to instantiate
 * the analysis, and provides some utility functionality.
 *
 * A modular analysis is a system of constraints with side effects. It splits the analysis of a program into the analyses of components
 * (`analyzeComponent`), which read (`get`) and write (`set`) parts of the global analysis state, called dependencies, and discover (`spawn`) other
 * components, through an `AnalysisState`. The analysis computes the least fixpoint of these analyses. How it does so is up to the fixpoint
 * algorithm that it mixes in, which keeps the global analysis state and provides the analysis states: it decides when to analyze which
 * components, when the changes of an analysis take effect, and how to update a dependency with the values that are written to it.
 */
abstract class ModAnalysis[Expr <: Expression](val program: Expr) {

    // parameterized by a component representation
    type Component
    def initialComponent: Component

    /**
     * A part of the global analysis state (such as the result of a component), which the analyses of components read and write. Each analysis defines
     * its own dependencies, each with a lattice of values.
     */
    trait Dependency extends SmartHash:
        type Value
        given lattice: Lattice[Value]

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

    /** Returns the value of `dep`, outside of the analyses of the components (e.g., to inspect the results of the analysis). */
    def value(dep: Dependency): dep.Value

    /** Returns the components that the analyses of the components discovered. */
    def components: Set[Component]

    // Running the analysis:

    /* Runs the analysis with a timeout. Implementation should be provided by the fixpoint algorithm. */
    protected def run(timeout: Timeout.T): Unit

    /** Initializes the analysis before it runs for the first time (e.g., the state of the fixpoint algorithm). */
    def init(): Unit = ()

    // flag to indicate if the analysis has already been initialized (see method `init`)
    private var initialized: Boolean = false

    /** Returns a boolean indicating whether the analysis has finished. Implementation should be provided by the fixpoint algorithm. */
    def finished: Boolean

    /**
     * Runs the analysis with a given timeout. Extra care should be taken when using this method, as the analysis results are not guaranteed to be
     * sound if the timeout is triggered. Therefore, it is recommended to explicitly check afterwards if the analysis terminated using the `finished`
     * method. Alternatively, one can use the `analyze` method to ensure the full (and sound) analysis result is computed.
     * @param timeout
     *   allows this method to return before termination of the analysis if the given timeout is reached
     */
    final def analyzeWithTimeout(timeout: Timeout.T): Unit =
        if !initialized then
            init()
            initialized = true
        run(timeout)

    /** Convenience method, akin to analyzeWithTimeout, but where the timeout is directly given in seconds */
    final def analyzeWithTimeoutInSeconds(seconds: Long): Unit =
        analyzeWithTimeout(Timeout.start(Duration(seconds, SECONDS)))

    /**
     * Runs the analysis. A timeout can not be configured (use `analyzeWithTimeout` instead if needed), meaning the method returns only when the
     * analysis has actually terminated. Therefore, this method ensures that the the full (and sound) analysis result is computed.
     */
    final def analyze(): Unit = analyzeWithTimeout(Timeout.none)

    /** Returns any metrics collected during the analysis, if any (default returns empty list) */
    def metrics: List[Metric] = List()
}
