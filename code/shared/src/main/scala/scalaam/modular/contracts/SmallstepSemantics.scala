package scalaam.modular.contracts

import scalaam.core.{Expression, LIFOWorkList, WorkList}
import scalaam.modular.{ModAnalysis, ReturnValue}
import scalaam.util.benchmarks.Timeout

trait SmallstepSemantics[Expr <: Expression] extends ModAnalysis[Expr] with ReturnValue[Expr] {

  override def intraAnalysis(cmp: Component): SmallstepSemanticsIntra
  trait SmallstepSemanticsIntra extends IntraAnalysis with ReturnResultIntra {
    type State

    override def analyze(timeout: Timeout.T): Unit = {
      var work: WorkList[State] = LIFOWorkList()
      var visited               = Set[State]()
      var result                = lattice.bottom

      work = work.add(initialState)

      while (work.nonEmpty) {
        val state = work.head
        work = work.tail
        if (isFinalState(state)) {
          result = lattice.join(result, finalStateResult(state))
        } else if (!visited.contains(state)) {
          val successors = step(state)
          work = work.addAll(successors)
          visited += state
        }
      }
      writeResult(result)
    }

    /**
      * The initial state of the semantics
      */
    def initialState: State

    /**
      * Checks whether the given state is a final state
      */
    def isFinalState(state: State): Boolean

    /**
      * Retrieves the result from the final state
      */
    def finalStateResult(state: State): Value

    /**
      * Evaluates the current state to a set of next states
      */
    def step(state: State): Set[State]
  }
}
