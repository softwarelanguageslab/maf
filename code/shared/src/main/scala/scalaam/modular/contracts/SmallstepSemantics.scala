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

    def isFinalState(state: State): Boolean
    def finalStateResult(state: State): Value
    def step(state: State): Set[State]
  }
}
