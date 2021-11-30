package maf.aam

import maf.core.*
import maf.util.graph.*
import maf.util.graph.Graph.GraphOps
import maf.util.benchmarks.Timeout

case class GraphElementAAM(hsh: Int, label: String, color: Color, data: String) extends GraphElement:
    def metadata: GraphMetadata = GraphMetadataString(data)

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

    /** A set of seen states in the analysis */
    private var seen: Set[State] = Set()

    /** A set of states still to visit */
    private var todo: Set[State] = Set()

    /** A set of all errors in the program */
    private var errors: Set[ErrorState] = Set()

    /** Initial timestamp */
    val initialTime: Timestamp

    /** Tick the time forward */
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: KonA): Timestamp

    /** Inject the expression into the analysis state */
    def inject(expr: Expr): State

    /** Step the analysis state */
    def step(start: State): Set[State]

    /** Invalidate the set of seen states */
    def invalidate(): Unit =
      seen = Set()

    /** Print a debug version of the given state */
    def printDebug(s: State, printStore: Boolean = false): Unit

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

    def loop[G](
        work: List[State],
        newWork: List[State],
        graph: G,
        timeout: Timeout.T
      )(using Graph[G, GraphElementAAM, GraphElement]
      ): (Set[State], G) =
      if (work.isEmpty && newWork.isEmpty) || (timeout.reached) || (false && (seen.size > 1000)) then (work.toSet ++ newWork.toSet, graph)
      else if work.isEmpty then loop(newWork, List(), graph, timeout)
      else if seen.contains(work.head) then loop(work.tail, newWork, graph, timeout)
      else
          seen = seen + work.head
          val todos = step(work.head)
          val (updatedGraph, newWork1) = todos.foldLeft((graph, newWork)) { case ((g, newWork), todo) =>
            val todoGe = asGraphElement(todo)
            val workGe = asGraphElement(work.head)
            val gUpdated = (if !seen.contains(todo) then g.addNode(todoGe) else g)
              .addEdge(workGe, NoTransition(), todoGe)

            (gUpdated, if !seen.contains(todo) then todo :: newWork else newWork)
          }

          loop(work.tail, newWork1, updatedGraph, timeout)

    /** Analyze the given expression and return the set of (non-invalid) state */
    def analyze[G](expr: Expr, graph: G, timeout: Timeout.T = Timeout.none)(using Graph[G, GraphElementAAM, GraphElement]): (Set[State], G) =
        val s0 = inject(expr)
        val (todos, g) = loop(List(s0), List(), graph, timeout)
        todo = todos
        (seen, g)

    def analyzeWithTimeout[G](timeout: Timeout.T, graph: G)(using Graph[G, GraphElementAAM, GraphElement]): (Set[State], G)

    def finished: Boolean = todo.isEmpty
