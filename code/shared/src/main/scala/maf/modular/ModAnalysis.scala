package maf.modular

import maf.core._
import maf.util.SmartHash
import maf.util.benchmarks.Timeout
import scala.concurrent.duration._

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import maf.util.graph.Graph.apply
import maf.util.graph.DotGraph

// an intra-analysis of a component can read ("register") or write ("trigger") dependencies
// a dependency represents a part of the global analysis state (such as a location in the global analysis' store)
// in essence, whenever a dependency is triggered, all registered components for that dependency need to be re-analyzed
trait Dependency extends SmartHash

case class Metric(name: String, result: Double)

/** Super type of all analyses in MAF, provides basic entry points to the analysis */
trait AnalysisEntry[Exp <: Expression]:
    /** The name of the analysis */
    val analysisName: String = "None"

    /** Returns a boolean indicating whether the analysis has finished. Implementation should be provided by the work list algorithm. */
    def finished: Boolean

    /** Runs the analysis until the given time-out passes */
    def analyzeWithTimeout(timeout: Timeout.T): Unit

    /** Runs the analysis without a time-out */
    def analyze(): Unit =
        analyzeWithTimeout(Timeout.none)

    /** Returns any metrics collected during the analysis, if any (default returns empty list) */
    def metrics: List[Metric] = List()

    /** The name of the program under analysis (if available) */
    def name: Option[String] = None

    /** The result of the analysis, if any is available */
    def result: Option[Any] = None

    /** Method that defines how to print the result of the analysis */
    def printResult: Unit = println(result)

    /**
     * This saves the current analysis to a file
     *
     * @param filename
     *   The file to save to
     */
    def save(filename: String): Unit = System.err.nn.println("Save functionality is not implemented for this analysis")

    /**
     * This saves the current analysis to a file, but only the elements that are in save
     *
     * @param filename
     *   The file to save to
     * @param save
     *   The elements to save
     */
    def save(filename: String, save: Set[String]): Unit = System.err.nn.println("Save functionality is not implemented for this analysis")

    /**
     * Load an analysis from a given file
     *
     * @param filename
     *   The file to load the analysis from
     */
    def load(filename: String): Unit = System.err.nn.println("Load functionality is not implemented for this analysis")

    /**
     * Load the given elements of an analysis from a given file
     *
     * @param filename
     *   The file to load the analysis from
     * @param load
     *   The elements to load
     */
    def load(filename: String, load: Set[String]): Unit = System.err.nn.println("Load functionality is not implemented for this analysis")

    /**
     * Method that renders a Dot graph of the components and the dependencies between them and writes it to a file
     *
     * @param filename
     *   the name of output file
     * @note
     *   this operation generates an empty dot graph by default
     */
    def toDot(filename: String): Unit =
        val g = DotGraph.empty
        g.toFile(filename)

/**
 * Base class of a modular analysis. Specifies the elements (fields, methods, and types) to be provided to instantiate the analysis, and provides some
 * utility functionality.
 */
abstract class ModAnalysis[Expr <: Expression](val program: Expr) extends Cloneable with Serializable with AnalysisEntry[Expr] { inter =>

    // parameterized by a component representation
    type Component <: Serializable
    def initialComponent: Component
    def expr(cmp: Component): Expr

    // some form of "worklist" is required to keep track of which components need to be (re-)analyzed
    // this method is responsible for adding a given component to that worklist
    def addToWorkList(cmp: Component): Unit
    // convenience method to add multiple components to the worklist
    def addToWorkList(cps: Iterable[Component]): Unit = cps.foreach(addToWorkList)

    // the intra-analysis of a component can discover new components
    // when we discover a component that has not yet been analyzed, we add it to the worklist
    // concretely, we keep track of a set `visited` of all components that have already been visited
    var visited: Set[Component] = Set()
    def spawn(cmp: Component, from: Component): Unit = spawn(cmp)
    def spawn(cmp: Component): Unit =
        if !visited(cmp) then // TODO[easy]: a mutable set could do visited.add(...) in a single call
            visited += cmp
            addToWorkList(cmp)

    /** Keeps track of the components depending on a given "effect" (~ read dependencies). */
    var deps: Map[Dependency, Set[Component]] = Map[Dependency, Set[Component]]().withDefaultValue(Set.empty)
    def register(target: Component, dep: Dependency): Unit = deps += dep -> (deps(dep) + target)
    def trigger(dep: Dependency): Unit = triggeredComponents(dep).foreach(addToWorkList)

    def triggeredComponents(dep: Dependency): Set[Component] = deps(dep)

    /**
     * Performs a deep copy of this analysis.
     * @note
     *   If subclasses introduce mutable state, the subclasses are responsible for correctly copying that state. Also note that 'vars' are copied
     *   correctly if the corresponding data structures are immutable.
     */
    def deepCopy(): this.type = this.clone().asInstanceOf[this.type]

    // parameterized by an 'intra-component analysis'
    def intraAnalysis(component: Component): IntraAnalysis
    abstract class IntraAnalysis(val component: Component):
        intra =>

        /** Set of dependencies read by this intra-component analysis. */
        var R: Set[Dependency] = Set()

        /** Set of dependencies written (triggered) by this intra-component analysis. */
        var W: Set[Dependency] = Set()

        /** Set of components discovered by this intra-component analysis. */
        var C: Set[Component] = Set()

        /** Registers a read dependency. */
        def register(dep: Dependency): Unit = R += dep

        /** Triggers a written dependency. */
        def trigger(dep: Dependency): Unit = W += dep

        /** Spawns a discovered component. */
        def spawn(cmp: Component): Unit = C += cmp

        /**
         * Performs the intra-component analysis of the given component.<br> <b>Important:</b> should only update the *local* analysis state, and must
         * not modify the global analysis state directly.
         */
        def analyzeWithTimeout(timeout: Timeout.T): Unit

        /** Pushes the local changes to the global analysis state. */
        def commit(): Unit =
            R.foreach(inter.register(component, _))
            W.foreach(dep => if doWrite(dep) then inter.trigger(dep))
            C.foreach(inter.spawn(_, component))

        /** Called upon a commit for every written dependency. Returns a boolean indicating whether the global analysis state was modified. */
        def doWrite(dep: Dependency): Boolean = throw new Exception(s"Unknown dependency $dep") // `ModAnalysis` has no knowledge of dependencies it can commit.

    // Specific to the worklist algorithm:

    // flag to indicate if the analysis has already been initialized (see method `init`)
    private var initialized: Boolean = false
    def analysisInitialized = initialized
    def analysisInitialized_=(init: Boolean) = initialized = init

    /* Runs the analysis with a timeout. Implementation should be provided by the worklist algorithm. */
    protected def run(timeout: Timeout.T): Unit

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
    final override def analyze(): Unit = analyzeWithTimeout(Timeout.none)

    def init() =
        visited = visited + initialComponent

    // Print analysis information.

    def configString(): String = "Modular analysis"
}
