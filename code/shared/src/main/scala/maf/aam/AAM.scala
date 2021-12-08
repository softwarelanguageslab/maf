package maf.aam

import maf.core.*
import maf.util.graph.*
import maf.util.graph.Graph.GraphOps
import maf.util.benchmarks.Timeout
import scala.annotation.tailrec

case class GraphElementAAM(hsh: Int, label: String, color: Color, data: String) extends GraphElement:
    def metadata: GraphMetadata = GraphMetadataString(data)

case class AnalysisResult[G, V, C](dependencyGraph: G, values: Set[V], allConfs: Set[C])

/** Provides functionality for a full AAM style analysis */
trait AAMAnalysis:
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

    /**
     * The type of configuration, in classic AAM w/o otimisations this is equal to the state. In optimized AAM ,various parts of the state that is
     * passed to the `step` function might be removed.
     */
    type Conf

    /** The components of our fixpoint system */
    type System <: BaseSystem

    /** The type of expression to use in the analysis */
    type Expr

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

    /** Decide what to do with the successor states */
    protected def decideSuccessors(succ: Set[State], sys: System): System

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
    def step(start: State): Set[State]

    /** Print a debug version of the given state */
    def printDebug(s: Conf, printStore: Boolean = false): Unit

    /** Compare two states, return true if they are equal */
    def compareStates(s1: State, s2: State): Boolean

    /** Checks whether the given state is a final state */
    def isFinal(st: State): Boolean

    /** Extracts the value of the given state (if any) */
    def extractValue(st: State): Option[Val]

    /** Allocate a fresh address in the store */
    def alloc(identity: Identity, env: Env, sto: Sto, kont: KonA, ctx: Timestamp): Address

    /** Represents the given state als an element in the graph */
    def asGraphElement(state: State): GraphElementAAM

    /** Register the error in the analysis */
    def registerError(error: Error, state: State): Unit =
      errors += ErrorState(error, state)

    /** Transition from one system to another (step the semantics) */
    protected def transition(sys: System): System

    /** Compute the fix point of the given system */
    protected def fix(timeout: Timeout.T)(sys: System): System =
        sys.reset()
        val next = transition(sys)
        if !next.hasChanged then
            /* fixpoint */
            finished = true
            sys
        else if timeout.reached then
            /* timeout */
            finished = false
            sys
        else fix(timeout)(next)

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
        val sys = fix(timeout)(s0)
        AnalysisResult(graph, sys.finalStates.flatMap(extractValue(_)), sys.allConfs)

    def analyzeWithTimeout[G](timeout: Timeout.T, graph: G)(using Graph[G, GraphElementAAM, GraphElement]): AnalysisResult[G, Val, Conf]

    var finished: Boolean = false
