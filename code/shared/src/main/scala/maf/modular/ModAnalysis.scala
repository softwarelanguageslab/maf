package maf.modular

import maf.core._
import maf.util.SmartHash
import maf.util.benchmarks.Timeout

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

// an intra-analysis of a component can read ("register") or write ("trigger") dependencies
// a dependency represents a part of the global analysis state (such as a location in the global analysis' store)
// in essence, whenever a dependency is triggered, all registered components for that dependency need to be re-analyzed
trait Dependency extends SmartHash

/**
 * Base class of a modular analysis. Specifies the elements (fields, methods, and types) to be provided to instantiate the analysis, and
 * provides some utility functionality.
 */
abstract class ModAnalysis[Expr <: Expression](prog: Expr) extends Cloneable with Serializable { inter =>

  // parameterized by a component representation
  type Component <: Serializable
  def initialComponent: Component
  def expr(cmp: Component): Expr

  // Retrieve a (possibly modified) version of the program
  def program: Expr = prog

  // some form of "worklist" is required to keep track of which components need to be (re-)analyzed
  // this method is responsible for adding a given component to that worklist
  def addToWorkList(cmp: Component): Unit

  // the intra-analysis of a component can discover new components
  // when we discover a component that has not yet been analyzed, we add it to the worklist
  // concretely, we keep track of a set `visited` of all components that have already been visited
  var visited: Set[Component] = Set()
  def spawn(cmp: Component, from: Component): Unit = spawn(cmp)
  def spawn(cmp: Component): Unit =
    if (!visited(cmp)) { // TODO[easy]: a mutable set could do visited.add(...) in a single call
      visited += cmp
      addToWorkList(cmp)
    }

  /** Keeps track of the components depending on a given "effect" (~ read dependencies). */
  var deps: Map[Dependency, Set[Component]] = Map[Dependency, Set[Component]]().withDefaultValue(Set.empty)
  def register(target: Component, dep: Dependency): Unit = deps += (dep -> (deps(dep) + target))
  def trigger(dep: Dependency): Unit = deps(dep).foreach(addToWorkList)

  /**
   * Performs a deep copy of this analysis.
   * @note If subclasses introduce mutable state, the subclasses are responsible for correctly copying that state. Also note that 'vars' are copied correctly if the corresponding data structures are immutable.
   */
  def deepCopy(): this.type = this.clone().asInstanceOf[this.type]

  // parameterized by an 'intra-component analysis'
  def intraAnalysis(component: Component): IntraAnalysis
  abstract class IntraAnalysis(val component: Component) { intra =>

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
     * Performs the intra-component analysis of the given component.<br>
     * <b>Important:</b> should only update the *local* analysis state, and must not modify the global analysis state directly.
     */
    def analyze(timeout: Timeout.T): Unit

    /** Pushes the local changes to the global analysis state. */
    def commit(): Unit = {
      R.foreach(inter.register(component, _))
      W.foreach(dep => if (doWrite(dep)) inter.trigger(dep))
      C.foreach(inter.spawn(_, component))
    }

    /** Called upon a commit for every written dependency. Returns a boolean indicating whether the global analysis state was modified. */
    def doWrite(dep: Dependency): Boolean = false // `ModAnalysis` has no knowledge of dependencies it can commit.
  }

  // Specific to the worklist algorithm:

  /** Returns a boolean indicating whether the analysis has finished. Implementation should be provided by the work list algorithm. */
  def finished(): Boolean

  /** Runs the analysis with an optional timeout. Implementation should be provided by the work list algorithm. */
  def analyze(timeout: Timeout.T): Unit // <= run the analysis (with given timeout)

  // Exporting an analysis to a file.

  /** Serialises a modular analysis and writes it to a file. */
  def exportAnalysis(file: String): Unit = {
    val out: ObjectOutputStream = new ObjectOutputStream(new FileOutputStream(file))
    out.writeObject(this)
    out.close()
  }

  def init() =
    visited = visited + initialComponent

  init()
}

object ModAnalysis {

  /** Reads a serialised modular analysis from a file and returns the deserialised object. */
  def importAnalysis[A <: ModAnalysis[_]](file: String): A = {
    val in: ObjectInputStream = new ObjectInputStream(new FileInputStream(file))
    val analysis: A = in.readObject().asInstanceOf[A]
    in.close()
    analysis
  }
}
