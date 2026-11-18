package maf.fixpoint.worklist

import maf.analysis.Analysis
import maf.fixpoint.{FixpointAlgorithm, Metric, Solver}
import maf.syntax.Expression
import maf.util.Timeout

/**
 * A fixpoint algorithm that keeps a worklist (of type `W`) of the components to (re)analyze: the components that are spawned for the first time, and
 * the ones that read a dependency when it changes. The worklist decides the order in which the components are analyzed. The analysis states change
 * the global analysis state right away, and update a dependency by joining the values that are written to it.
 */
class WorklistAlgorithm[W[_]: Worklist] extends FixpointAlgorithm:
    def solver[E <: Expression](anl: Analysis[E], prg: E): Solver[anl.type] = WorklistSolver[W, E, anl.type](anl, prg)

/** A worklist algorithm that analyzes the components in a breadth-first order. */
object FIFOWorklistAlgorithm extends WorklistAlgorithm[FIFOWorklist]

/** A worklist algorithm that analyzes the components in a depth-first order. */
object LIFOWorklistAlgorithm extends WorklistAlgorithm[LIFOWorklist]

/** A worklist algorithm that analyzes the components in an arbitrary order. */
object RandomWorklistAlgorithm extends WorklistAlgorithm[RandomWorklist]

/** A solver of `analysis` for the program `prg`, which uses a worklist of type `W`. */
private class WorklistSolver[W[_]: Worklist, E <: Expression, A <: Analysis[E]](val analysis: A, prg: E) extends Solver[A]:
    import analysis.{Component, Dependency}

    // the component of the program, which the analysis starts from
    private val initial = analysis.initialComponent(prg)

    /** The values of the dependencies that were written (the others are still at the bottom of their lattice). */
    private var values: Map[Dependency, Any] = Map()

    /** For every dependency, the components that read it. */
    private var readers: Map[Dependency, Set[Component]] = Map().withDefaultValue(Set.empty)

    /** The components that were spawned (at first, only the initial one), which are added to the worklist only the first time. */
    private var visited: Set[Component] = Set(initial)

    /** The components to (re)analyze (at first, only the initial one). */
    private var worklist: W[Component] = summon[Worklist[W]].empty[Component].add(initial)

    private def addToWorklist(cmp: Component): Unit = worklist = worklist.add(cmp)

    /** An analysis state for the analysis of `cmp`, which changes the global analysis state right away. */
    private class State(cmp: Component) extends analysis.AnalysisState:
        def spawn(other: Component): Unit =
            if !visited(other) then // TODO[easy]: a mutable set could do visited.add(...) in a single call
                visited += other
                addToWorklist(other)
        def get(dep: Dependency): dep.Value =
            readers += dep -> (readers(dep) + cmp)
            value(dep)
        def set(dep: Dependency, vlu: dep.Value): Unit =
            val old = value(dep)
            val upd = dep.lattice.join(old, vlu)
            if upd != old then
                values += dep -> upd
                readers(dep).foreach(addToWorklist)

    // the number of analyses of components so far
    private var iterations: Int = 0

    /** Analyzes the components in the worklist until it is empty or the timeout is reached. */
    def solve(timeout: Timeout.T): Unit =
        while !worklist.isEmpty && !timeout.reached do
            val (cmp, rest) = worklist.next
            worklist = rest
            iterations += 1
            analysis.analyzeComponent(cmp, State(cmp))

    def finished: Boolean = worklist.isEmpty
    def value(dep: Dependency): dep.Value = values.get(dep) match
        case Some(vlu) => vlu.asInstanceOf[dep.Value]
        case None      => dep.lattice.bottom
    def components: Set[Component] = visited
    def dependencies: Set[Dependency] = values.keySet
    def metrics: List[Metric] = List(Metric("iterations", iterations))
