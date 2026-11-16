package maf.fixpoint

import maf.util.Timeout

import scala.annotation.unused
import scala.collection.mutable.PriorityQueue

/**
 * A fixpoint algorithm that keeps a worklist of the components to (re)analyze: the components that are spawned for the first time, and the ones that
 * read a dependency when it changes. Its analysis states change the global analysis state right away, and update a dependency by joining the values
 * that are written to it.
 */
trait WorklistAlgorithm extends ModAnalysis[?]:
    def addToWorkList(cmp: Component): Unit

    /** The values of the dependencies that were written (the others are still at the bottom of their lattice). */
    var values: Map[Dependency, Any] = Map()
    def value(dep: Dependency): dep.Value = values.get(dep) match
        case Some(vlu) => vlu.asInstanceOf[dep.Value]
        case None      => dep.lattice.bottom

    /** For every dependency, the components that read it. */
    var deps: Map[Dependency, Set[Component]] = Map[Dependency, Set[Component]]().withDefaultValue(Set.empty)
    def register(target: Component, dep: Dependency): Unit = deps += dep -> (deps(dep) + target)

    /** The components that were spawned, which are added to the worklist only the first time. */
    var visited: Set[Component] = Set()
    def components: Set[Component] = visited

    override def init(): Unit =
        super.init()
        visited += initialComponent

    protected def spawn(cmp: Component, @unused from: Component): Unit = spawn(cmp)
    protected def spawn(cmp: Component): Unit =
        if !visited(cmp) then // TODO[easy]: a mutable set could do visited.add(...) in a single call
            visited += cmp
            addToWorkList(cmp)

    /** An analysis state for the analysis of `cmp`, which changes the global analysis state right away. */
    class WorklistAnalysisState(cmp: Component) extends AnalysisState:
        def spawn(other: Component): Unit = WorklistAlgorithm.this.spawn(other, cmp)
        def get(dep: Dependency): dep.Value =
            register(cmp, dep)
            value(dep)
        def set(dep: Dependency, vlu: dep.Value): Unit =
            val old = value(dep)
            val upd = dep.lattice.join(old, vlu)
            if upd != old then
                values += dep -> upd
                deps(dep).foreach(addToWorkList)

trait SequentialWorklistAlgorithm extends WorklistAlgorithm:
    // we can choose what kind of worklist to pick
    def emptyWorkList: WorkList[Component]
    // adding elements to the worklist
    var workList: WorkList[Component] = emptyWorkList.add(initialComponent)
    def addToWorkList(cmp: Component) = workList = workList.add(cmp)
    def finished: Boolean = workList.isEmpty
    // a single step in the worklist algorithm iteration
    def step(): Unit =
        // take the next component
        val current = workList.head
        workList = workList.tail
        // analyze it
        analyzeComponent(current, WorklistAnalysisState(current))

    // step until worklist is empty or timeout is reached
    def run(timeout: Timeout.T): Unit =
        while !finished && !timeout.reached do step()

    // add the initial component during initialisation
    override def init() =
        super.init()
        addToWorkList(initialComponent)

/** Provides a work list with a depth-first exploration order to a modular analysis. */
trait LIFOWorklistAlgorithm extends SequentialWorklistAlgorithm:
    def emptyWorkList = LIFOWorkList()

/** Provides a work list with a breadth-first exploration order to a modular analysis. */
trait FIFOWorklistAlgorithm extends SequentialWorklistAlgorithm:
    def emptyWorkList = FIFOWorkList()

/** Provides a work list with a random exploration order to a modular analysis. */
trait RandomWorklistAlgorithm extends SequentialWorklistAlgorithm:
    def emptyWorkList = RandomWorkList()

// TODO: use an immutable priority queue, or reuse SequentialWorklistAlgorithm differently here
trait PriorityQueueWorklistAlgorithm extends WorklistAlgorithm:
    // choose the priority ordering of components
    implicit lazy val ordering: Ordering[Component]
    // worklist is a priority queue
    var worklistSet: Set[Component] = Set(initialComponent)
    lazy val worklist: PriorityQueue[Component] = PriorityQueue(initialComponent)
    def push(cmp: Component) =
        if !worklistSet.contains(cmp) then
            worklistSet += cmp
            worklist += cmp
    def pop(): Component =
        val cmp = worklist.dequeue()
        worklistSet -= cmp
        cmp
    def addToWorkList(cmp: Component): Unit = push(cmp)
    def finished: Boolean = worklist.isEmpty
    // a single step in the worklist algorithm iteration
    def step(): Unit =
        // take the next component, and analyze it
        val current = pop()
        analyzeComponent(current, WorklistAnalysisState(current))

    // step until worklist is empty or timeout is reached
    def run(timeout: Timeout.T): Unit =
        while !finished && !timeout.reached do step()


/** Provides a work list that prioritises nested calls by call depth to a modular analysis. */
trait CallDepthFirstWorklistAlgorithm extends PriorityQueueWorklistAlgorithm:
    var depth: Map[Component, Int] = Map.empty.withDefaultValue(0)
    lazy val ordering: Ordering[Component] = Ordering.by(depth)
    override protected def spawn(cmp: Component, from: Component): Unit =
        if !visited(cmp) then // TODO[easy]: a mutable set could do visited.add(...) in a single call
            visited += cmp
            depth += cmp -> (depth(from) + 1)
            addToWorkList(cmp)

trait LeastVisitedFirstWorklistAlgorithm extends PriorityQueueWorklistAlgorithm:
    var count: Map[Component, Int] = Map.empty.withDefaultValue(0)
    lazy val ordering: Ordering[Component] = Ordering.by(count).reverse
    override def pop(): Component =
        val cmp = super.pop()
        count += cmp -> (count(cmp) + 1)
        cmp

trait MostVisitedFirstWorklistAlgorithm extends LeastVisitedFirstWorklistAlgorithm:
    override lazy val ordering: Ordering[Component] = Ordering.by(count)

trait MostDependenciesFirstWorklistAlgorithm extends PriorityQueueWorklistAlgorithm:
    var cmpDeps: Map[Component, Set[Dependency]] = Map.empty.withDefaultValue(Set.empty)
    var depCount: Map[Component, Int] = Map.empty.withDefaultValue(0)
    lazy val ordering: Ordering[Component] = Ordering.by(depCount)
    override def register(cmp: Component, dep: Dependency): Unit =
        super.register(cmp, dep)
        cmpDeps += (cmp -> (cmpDeps(cmp) + dep))
        depCount += (cmp -> (cmpDeps(cmp).size))

trait LeastDependenciesFirstWorklistAlgorithm extends MostDependenciesFirstWorklistAlgorithm:
    override lazy val ordering: Ordering[Component] = Ordering.by(depCount).reverse

trait BiggerEnvironmentFirstWorklistAlgorithm extends PriorityQueueWorklistAlgorithm:
    def environmentSize(cmp: Component): Int
    lazy val ordering: Ordering[Component] = Ordering.by(environmentSize)

trait SmallerEnvironmentFirstWorklistAlgorithm extends BiggerEnvironmentFirstWorklistAlgorithm:
    override lazy val ordering: Ordering[Component] = Ordering.by(environmentSize).reverse
