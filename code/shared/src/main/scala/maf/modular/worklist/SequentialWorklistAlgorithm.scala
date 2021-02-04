package maf.modular.worklist

import maf.core._
import maf.core.worklist.{FIFOWorkList, LIFOWorkList, RandomWorkList, WorkList}
import maf.modular.ModAnalysis
import maf.util.benchmarks.Timeout

import scala.collection.mutable.PriorityQueue
import maf.modular.Dependency

trait SequentialWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // we can choose what kind of worklist to pick
  def emptyWorkList: WorkList[Component]
  // adding elements to the worklist
  var workList: WorkList[Component] = emptyWorkList.add(initialComponent)
  def addToWorkList(cmp: Component) = workList = workList.add(cmp)
  def finished(): Boolean = workList.isEmpty
  // a single step in the worklist algorithm iteration
  // var intraCount: Long = 0L
  def step(timeout: Timeout.T): Unit = {
    // take the next component
    val current = workList.head
    workList = workList.tail
    // do the intra-analysis
    // intraCount = intraCount + 1
    val intra = intraAnalysis(current)
    intra.analyzeWithTimeout(timeout)
    if (timeout.reached) {
      // analysis timed out => we need to add it to the worklist again
      addToWorkList(current)
    } else {
      // analysis finished properly => commit its changes to the global analysis state
      intra.commit()
    }
  }

  // step until worklist is empty or timeout is reached
  def analyzeWithTimeout(timeout: Timeout.T): Unit =
    while (!finished() && !timeout.reached)
      step(timeout)
}

/** Provides a work list with a depth-first exploration order to a modular analysis. */
trait LIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = LIFOWorkList()
}

/** Provides a work list with a breadth-first exploration order to a modular analysis. */
trait FIFOWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = FIFOWorkList()
}

/** Provides a work list with a random exploration order to a modular analysis. */
trait RandomWorklistAlgorithm[Expr <: Expression] extends SequentialWorklistAlgorithm[Expr] {
  def emptyWorkList = RandomWorkList()
}

// TODO: use an immutable priority queue, or reuse SequentialWorklistAlgorithm differently here
trait PriorityQueueWorklistAlgorithm[Expr <: Expression] extends ModAnalysis[Expr] {
  // choose the priority ordering of components
  implicit val ordering: Ordering[Component]
  // worklist is a priority queue
  var worklistSet: Set[Component] = Set(initialComponent)
  lazy val worklist: PriorityQueue[Component] = PriorityQueue(initialComponent)
  def push(cmp: Component) =
    if (!worklistSet.contains(cmp)) {
      worklistSet += cmp
      worklist += cmp
    }
  def pop(): Component = {
    val cmp = worklist.dequeue()
    worklistSet -= cmp
    cmp
  }
  def addToWorkList(cmp: Component): Unit = push(cmp)
  def finished(): Boolean = worklist.isEmpty
  // a single step in the worklist algorithm iteration
  def step(timeout: Timeout.T): Unit = {
    // take the next component
    val current = pop()
    // do the intra-analysis
    val intra = intraAnalysis(current)
    intra.analyzeWithTimeout(timeout)
    if (timeout.reached) {
      // analysis timed out => we need to add it to the worklist again
      addToWorkList(current)
    } else {
      // analysis finished properly => commit its changes to the global analysis state
      intra.commit()
    }
  }

  // step until worklist is empty or timeout is reached
  def analyzeWithTimeout(timeout: Timeout.T): Unit =
    while (!finished() && !timeout.reached)
      step(timeout)
}

/** Provides a work list that prioritises nested calls by call depth to a modular analysis. */
trait CallDepthFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  var depth: Map[Component, Int] = Map.empty.withDefaultValue(0)
  lazy val ordering: Ordering[Component] = Ordering.by(depth)
  override def spawn(cmp: Component, from: Component): Unit =
    if (!visited(cmp)) { // TODO[easy]: a mutable set could do visited.add(...) in a single call
      visited += cmp
      depth += cmp -> (depth(from) + 1)
      addToWorkList(cmp)
    }
}

trait LeastVisitedFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  var count: Map[Component, Int] = Map.empty.withDefaultValue(0)
  lazy val ordering: Ordering[Component] = Ordering.by(count).reverse
  override def pop(): Component = {
    val cmp = super.pop()
    count += cmp -> (count(cmp) + 1)
    cmp
  }
}

trait MostVisitedFirstWorklistAlgorithm[Expr <: Expression] extends LeastVisitedFirstWorklistAlgorithm[Expr] {
  override lazy val ordering: Ordering[Component] = Ordering.by(count)
}

trait DeepExpressionsFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  def computeDepths(exp: Expression, depths: Map[Identity, Int] = Map.empty): Map[Identity, Int] =
    exp.subexpressions.foldLeft(depths)((depths, exp) => computeDepths(exp, depths)).map({ case (k, v) => (k, v + 1) }) + (exp.idn -> 0)
  val depths: Map[Identity, Int] = computeDepths(program)
  var cmps: Map[Component, Int] = Map.empty.withDefaultValue(0)
  lazy val ordering: Ordering[Component] = Ordering.by(cmps)
  override def pop(): Component = {
    val cmp = super.pop()
    cmps += cmp -> depths(expr(cmp).idn)
    cmp
  }
}

trait ShallowExpressionsFirstWorklistAlgorithm[Expr <: Expression] extends DeepExpressionsFirstWorklistAlgorithm[Expr] {
  override lazy val ordering: Ordering[Component] = Ordering.by(cmps).reverse
}

trait MostDependenciesFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  var cmpDeps: Map[Component, Set[Dependency]] = Map.empty.withDefaultValue(Set.empty)
  var depCount: Map[Component, Int] = Map.empty.withDefaultValue(0)
  lazy val ordering: Ordering[Component] = Ordering.by(depCount)
  override def register(cmp: Component, dep: Dependency): Unit = {
    super.register(cmp, dep)
    cmpDeps += (cmp -> (cmpDeps(cmp) + dep))
    depCount += (cmp -> (cmpDeps(cmp).size))
  }
}

trait LeastDependenciesFirstWorklistAlgorithm[Expr <: Expression] extends MostDependenciesFirstWorklistAlgorithm[Expr] {
  override lazy val ordering: Ordering[Component] = Ordering.by(depCount).reverse
}

trait BiggerEnvironmentFirstWorklistAlgorithm[Expr <: Expression] extends PriorityQueueWorklistAlgorithm[Expr] {
  def environmentSize(cmp: Component): Int
  lazy val ordering: Ordering[Component] = Ordering.by(environmentSize)
}

object BiggerEnvironmentFirstWorklistAlgorithm {
  import maf.modular.scheme.modf._
  import maf.modular.scheme.modf.SchemeModFComponent._
  import maf.language.scheme._
  trait ModF extends BiggerEnvironmentFirstWorklistAlgorithm[SchemeExp] with StandardSchemeModFComponents {
    def environmentSize(cmp: Component): Int = cmp match {
      case Main                 => 0
      case Call((_, env), _, _) => env.size
    }
  }

  import maf.modular.scheme.modconc._
  trait ModConc extends BiggerEnvironmentFirstWorklistAlgorithm[SchemeExp] with StandardSchemeModConcComponents {
    def environmentSize(cmp: Component): Int = cmp match {
      case MainThread        => 0
      case Thread(_, env, _) => env.size
    }
  }
}
