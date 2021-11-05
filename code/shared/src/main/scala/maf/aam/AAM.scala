package maf.aam

import maf.core.*
import maf.util.benchmarks.Timeout

/** Provides functionality for a full AAM style analysis */
trait AAMAnalysis:
    /** The type of the abstract values for the analysis */
    type Val

    /** The type of the environment that should be used in the analysis */
    type Env

    /** The type of the closure that should be used in the analysis */
    type Clo

    /** The type of continuation that should be used in the analysis */
    type Kont

    /** The type of state that should be used in the analysis. */
    type State

    /** The type of expression to use in the analysis */
    type Expr

    /** The type of the timestamp in the analysis */
    type Timestamp

    /** The type of the store */
    type Sto

    /** A set of seen states in the analysis */
    private var seen: Set[State] = Set()

    /** A set of states still to visit */
    private var todo: Set[State] = Set()

    /** Initial timestamp */
    val initialTime: Timestamp

    /** Tick the time forward */
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: Kont): Timestamp

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

    /** Allocate a fresh address in the store */
    def alloc(identity: Identity, env: Env, sto: Sto, kont: Kont, ctx: Timestamp): Address

    /** Analyze the given expression and return the set of (non-invalid) state */
    def analyze(expr: Expr, timeout: Timeout.T = Timeout.none): Set[State] =
        val s0 = inject(expr)
        seen = Set(s0)
        todo = step(s0)
        while !todo.isEmpty && !timeout.reached && seen.size < 400 do
            println(s"todo size ${todo.size} and seen size ${seen.size}")
            seen = seen ++ todo
            todo = todo.flatMap(step)
            val size0 = todo.size
            todo = todo -- seen
            if size0 != todo.size then println(s"diff $size0 and ${todo.size}")
            todo.foreach(printDebug(_, false))

            for {
              st <- todo
              st2 <- todo
            } do () //compareStates(st, st2)

        todo = (todo -- seen)
        seen

    def analyzeWithTimeout(timeout: Timeout.T): Set[State]

    def finished: Boolean = todo.isEmpty
