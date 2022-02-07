package maf.aam

import maf.core.*
import maf.util.graph.*
import maf.util.graph.Graph.GraphOps
import maf.util.benchmarks.Timeout
import scala.annotation.tailrec
import maf.util.Trampoline
import maf.modular.AnalysisEntry

case class GraphElementAAM(hsh: Int, label: String, color: Color, data: String) extends GraphElement:
    def metadata: GraphMetadata = GraphMetadataString(data)
    override def hashCode: Int = hsh
    override def equals(other: Any): Boolean =
      this.hashCode == other.hashCode

case class AnalysisResult[G, V, C](dependencyGraph: G, values: Set[V], allConfs: Set[C])

type AAMGraph[G] = Graph[G, GraphElementAAM, GraphElement]

/** Provides functionality for a full AAM style analysis */
trait AAMAnalysis[E <: Expression] extends AnalysisEntry[E]:
    /** The type of the abstract values for the analysis */
    type Val
    type LatVal

    /** The type of the environment that should be used in the analysis */
    type Env

    /** The type of the closure that should be used in the analysis */
    type Clo

    /** The type of continuation that should be used in the analysis */
    type Kont
    type KonA = Kont | Address

    /** The type of state that should be used in the analysis. */
    type State

    /** The type of the result, we use trampolines here to avoid stackoverflow exceptions */
    protected type Result = Trampoline[Set[State]]
    protected type SingleResult = Trampoline[State]

    /**
     * The type of configuration, in classic AAM w/o otimisations this is equal to the state. In optimized AAM ,various parts of the state that is
     * passed to the `step` function might be removed.
     */
    type Conf

    /** The components of our fixpoint system */
    type System <: BaseSystem

    /** The type of expression to use in the analysis */
    type Expr <: E

    /** The type of the timestamp in the analysis */
    type Timestamp

    /** The type of the store */
    type Sto

    /** The type of errors in our analysis */
    type Error

    /** An error state */
    case class ErrorState(error: Error, state: State)

    /** Base system */
    trait BaseSystem:
        def finalStates: Set[State]
        def allConfs: Set[Conf]
        var hasChanged: Boolean = false
        def change[X](f: => X): X =
            hasChanged = true
            f
        def reset(): Unit = hasChanged = false

    /** A set of all errors in the program */
    private var errors: Set[ErrorState] = Set()

    /** Initial timestamp */
    val initialTime: Timestamp

    /** Tick the time forward */
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: KonA): Timestamp

    /** Inject the expression into the analysis system */
    protected def inject(expr: Expr): System

    /** Inject the expression into the analysis state */
    def injectConf(expr: Expr): Conf

    /** Step the analysis state */
    def step(start: State): Result

    /** Print a debug version of the given state */
    def printDebug(s: Conf, printStore: Boolean = false): Unit

    /** Compare two states, return true if they are equal */
    def compareStates(s1: Conf, s2: Conf): Boolean

    /** Checks whether the given state is a final state */
    def isFinal(st: State): Boolean

    /** Extracts the value of the given state (if any) */
    def extractValue(st: State): Option[Val]

    /** Allocate a fresh address in the store */
    def alloc(identity: Identity, env: Env, sto: Sto, kont: KonA, ctx: Timestamp): Address

    /** Represents the given state als an element in the graph */
    def asGraphElement(state: Conf, sys: System): GraphElementAAM

    /** Register the error in the analysis */
    def registerError(error: Error, state: State): Unit =
      errors += ErrorState(error, state)

    /** Transition from one system to another (step the semantics) */
    protected def transition[G](sys: System, dependencyGraph: G)(using AAMGraph[G]): (System, G)

    /** Compute the fix point of the given system */
    protected def fix[G](timeout: Timeout.T)(sys: System, dependencyGraph: G, iters: Int = 0)(using AAMGraph[G]): (System, G) =
        sys.reset()
        val (next, fpdg) = transition(sys, dependencyGraph)
        if !next.hasChanged then
            /* fixpoint */
            finished = true
            (next, fpdg)
        else if timeout.reached then
            /* timeout */
            finished = false
            (sys, dependencyGraph)
        else fix(timeout)(next, fpdg, iters + 1)

    /** Inject the configuration into a state that can be used for the small step semantics */
    protected def asState(conf: Conf, sys: System): State

    /** Reduces a state to a configuration */
    protected def asConf(state: State, sys: System): Conf

    /** Analyze the given expression and return the set of (non-invalid) state */
    def analyze[G](
        expr: Expr,
        graph: G,
        timeout: Timeout.T = Timeout.none
      )(using Graph[G, GraphElementAAM, GraphElement]
      ): AnalysisResult[G, Val, Conf] =
        val s0 = inject(expr)
        val (sys, graph1) = fix(timeout)(s0, graph)
        AnalysisResult(graph1, sys.finalStates.flatMap(extractValue(_)), sys.allConfs)

    def analyzeWithTimeout[G](timeout: Timeout.T, graph: G)(using Graph[G, GraphElementAAM, GraphElement]): AnalysisResult[G, Val, Conf]

    /** Runs the analysis until the given time-out passes */
    def analyzeWithTimeout(timeout: Timeout.T): Unit =
        val g = new NoGraph[GraphElementAAM, GraphElement]
        analyzeWithTimeout(timeout, g.G())(using g.G.typeclass)

    var finished: Boolean = false
