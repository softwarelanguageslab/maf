package maf.fixpoint

import maf.analysis.{Analysis, Metric}
import maf.syntax.Expression
import maf.util.Timeout

import scala.annotation.unused
import scala.collection.mutable.PriorityQueue

// The worklist algorithms keep a worklist of the components to (re)analyze: the components that are spawned for the first time, and the ones that
// read a dependency when it changes. Their analysis states change the global analysis state right away, and update a dependency by joining the
// values that are written to it. They differ in the order in which they analyze the components in their worklist.

/** A worklist algorithm that analyzes the components in a breadth-first order. */
object FIFOWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new SequentialWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def emptyWorkList = FIFOWorkList.empty

/** A worklist algorithm that analyzes the components in a depth-first order. */
object LIFOWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new SequentialWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def emptyWorkList = LIFOWorkList.empty

/** A worklist algorithm that analyzes the components in an arbitrary order. */
object RandomWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new SequentialWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def emptyWorkList = RandomWorkList.empty

/** A worklist algorithm that analyzes the most deeply nested calls first (the components that are spawned the furthest from the initial one). */
object CallDepthFirstWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new PriorityWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            private var depth: Map[anl.Component, Int] = Map.empty.withDefaultValue(0)
            protected def ordering = Ordering.by(depth(_))
            override protected def spawn(cmp: anl.Component, from: anl.Component): Unit =
                if !visited(cmp) then depth += cmp -> (depth(from) + 1)
                super.spawn(cmp, from)

/** A worklist algorithm that analyzes the components that it analyzed the least often first. */
object LeastVisitedFirstWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new AnalysisCountWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def ordering = Ordering.by((cmp: anl.Component) => timesAnalyzed(cmp)).reverse

/** A worklist algorithm that analyzes the components that it analyzed the most often first. */
object MostVisitedFirstWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new AnalysisCountWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def ordering = Ordering.by(timesAnalyzed(_))

/** A worklist algorithm that analyzes the components that read the most dependencies first. */
object MostDependenciesFirstWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new DependencyCountWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def ordering = Ordering.by(dependenciesRead(_).size)

/** A worklist algorithm that analyzes the components that read the fewest dependencies first. */
object LeastDependenciesFirstWorklistAlgorithm extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): anl.Solver =
        new DependencyCountWorklistSolver[anl.type](anl, anl.initialComponent(prg)) with anl.Solver:
            protected def ordering = Ordering.by((cmp: anl.Component) => dependenciesRead(cmp).size).reverse

// The solvers of the worklist algorithms. They implement the members of `analysis.Solver`, which each worklist algorithm mixes in: a class can
// not extend `analysis.Solver` itself, as the type checker could not tell that a solver of `anl` is an `anl.Solver`.

/** A solver of `analysis` for the program of the component `initial`, which uses a worklist. */
private[fixpoint] abstract class WorklistSolver[A <: Analysis[?]](val analysis: A, initial: analysis.Component):
    import analysis.Dependency

    /** Adds `cmp` to the worklist. */
    protected def add(cmp: analysis.Component): Unit

    /** Removes the next component to analyze from the worklist, and returns it. */
    protected def next(): analysis.Component

    /** Returns whether the worklist is empty. */
    protected def isEmpty: Boolean

    /** The values of the dependencies that were written (the others are still at the bottom of their lattice). */
    private var values: Map[Dependency, Any] = Map()

    /** For every dependency, the components that read it. */
    private var readers: Map[Dependency, Set[analysis.Component]] = Map().withDefaultValue(Set.empty)
    protected def register(cmp: analysis.Component, dep: Dependency): Unit = readers += dep -> (readers(dep) + cmp)

    /** The components that were spawned, which are added to the worklist only the first time. */
    protected var visited: Set[analysis.Component] = Set()
    protected def spawn(cmp: analysis.Component, @unused from: analysis.Component): Unit =
        if !visited(cmp) then // TODO[easy]: a mutable set could do visited.add(...) in a single call
            visited += cmp
            add(cmp)

    /** An analysis state for the analysis of `cmp`, which changes the global analysis state right away. */
    private class State(cmp: analysis.Component) extends analysis.AnalysisState:
        def spawn(other: analysis.Component): Unit = WorklistSolver.this.spawn(other, cmp)
        def get(dep: Dependency): dep.Value =
            register(cmp, dep)
            value(dep)
        def set(dep: Dependency, vlu: dep.Value): Unit =
            val old = value(dep)
            val upd = dep.lattice.join(old, vlu)
            if upd != old then
                values += dep -> upd
                readers(dep).foreach(add)

    // whether the solver has started, i.e., whether the initial component was added to the worklist
    private var started: Boolean = false

    // the number of analyses of components so far
    private var iterations: Int = 0

    /** Analyzes the components in the worklist until it is empty or the timeout is reached. */
    final def solve(timeout: Timeout.T): Unit =
        if !started then
            started = true
            visited += initial
            add(initial)
        while !isEmpty && !timeout.reached do
            val cmp = next()
            iterations += 1
            analysis.analyzeComponent(cmp, State(cmp))

    final def finished: Boolean = started && isEmpty
    final def value(dep: Dependency): dep.Value = values.get(dep) match
        case Some(vlu) => vlu.asInstanceOf[dep.Value]
        case None      => dep.lattice.bottom
    final def components: Set[analysis.Component] = visited
    final def dependencies: Set[Dependency] = values.keySet
    final def metrics: List[Metric] = List(Metric("iterations", iterations))

/** A solver with a worklist that is a `WorkList`. */
private[fixpoint] abstract class SequentialWorklistSolver[A <: Analysis[?]](anl: A, initial: anl.Component) extends WorklistSolver[A](anl, initial):
    protected def emptyWorkList: WorkList[analysis.Component]
    private var workList: WorkList[analysis.Component] = emptyWorkList
    protected def add(cmp: analysis.Component): Unit = workList = workList.add(cmp)
    protected def next(): analysis.Component =
        val cmp = workList.head
        workList = workList.tail
        cmp
    protected def isEmpty: Boolean = workList.isEmpty

// TODO: use an immutable priority queue
/** A solver with a worklist that is a priority queue, which contains each component at most once. */
private[fixpoint] abstract class PriorityWorklistSolver[A <: Analysis[?]](anl: A, initial: anl.Component) extends WorklistSolver[A](anl, initial):
    /** The order of the components, of which the greatest is analyzed first. */
    protected def ordering: Ordering[analysis.Component]
    private lazy val queue: PriorityQueue[analysis.Component] = PriorityQueue.empty(using ordering)
    private var queued: Set[analysis.Component] = Set()
    protected def add(cmp: analysis.Component): Unit =
        if !queued.contains(cmp) then
            queued += cmp
            queue += cmp
    protected def next(): analysis.Component =
        val cmp = queue.dequeue()
        queued -= cmp
        cmp
    protected def isEmpty: Boolean = queue.isEmpty

/** A solver with a priority queue that counts how often it analyzed each component. */
private[fixpoint] abstract class AnalysisCountWorklistSolver[A <: Analysis[?]](anl: A, initial: anl.Component) extends PriorityWorklistSolver[A](anl, initial):
    protected var timesAnalyzed: Map[analysis.Component, Int] = Map.empty.withDefaultValue(0)
    override protected def next(): analysis.Component =
        val cmp = super.next()
        timesAnalyzed += cmp -> (timesAnalyzed(cmp) + 1)
        cmp

/** A solver with a priority queue that keeps the dependencies that each component read. */
private[fixpoint] abstract class DependencyCountWorklistSolver[A <: Analysis[?]](anl: A, initial: anl.Component) extends PriorityWorklistSolver[A](anl, initial):
    protected var dependenciesRead: Map[analysis.Component, Set[analysis.Dependency]] = Map.empty.withDefaultValue(Set.empty)
    override protected def register(cmp: analysis.Component, dep: analysis.Dependency): Unit =
        super.register(cmp, dep)
        dependenciesRead += cmp -> (dependenciesRead(cmp) + dep)
