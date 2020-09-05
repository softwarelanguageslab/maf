package scalaam.modular.contracts
import scalaam.core.Position.Position
import scalaam.language.contracts.{ScBegin, ScExp, ScIdentifier, ScIf, ScLetRec, ScRaise, ScSet}

trait ScSmallStepSemantics extends ScModSemantics with SmallstepSemantics[ScExp] {
  def raiseMessage(message: String): Unit

  override def intraAnalysis(component: Component): IntraScSmallStepSemantics
  trait IntraScSmallStepSemantics extends IntraScAnalysis with SmallstepSemanticsIntra {
    case class S(kont: List[ScFrame]) {
      def push(k: ScFrame): S =
        this.copy(kont = k :: kont)
      def pop(): (ScFrame, S) =
        (kont.head, this.copy(kont = kont.tail))
    }

    sealed trait ScState
    case class EvalState(exp: ScExp, env: Env, state: S)            extends ScState
    case class ApplyKont(value: Value, state: S)                    extends ScState
    case class RaiseError(message: String, pos: Position, state: S) extends ScState

    sealed trait ScFrame
    case class IfFrame(consequent: ScExp, alternative: ScExp, state: S) extends ScFrame
    case class LetrecFrame(body: ScExp, state: S)                       extends ScFrame
    case class SeqFrame(sequence: List[ScExp], state: S)                extends ScFrame
    case class SetFrame(ident: ScIdentifier, state: S)                  extends ScFrame

    type State = ScState
    override def step(state: State): Set[State] = state match {
      case EvalState(exp, env, state) => eval(exp, env, state)
      case ApplyKont(_, S(List()))    => ??? // impossible
      case ApplyKont(value, S(kont))  => applyKont(value, kont.head, kont.tail)
    }

    override def isFinalState(state: ScState): Boolean   = ???
    override def finalStateResult(state: ScState): Value = ???

    private def eval(exp: ScExp, env: Env, state: S): Set[State] = exp match {
      case ScBegin(expressions, _) =>
        val k = SeqFrame(expressions.tail, state)
        Set(EvalState(expressions.head, env, state.push(k)))

      case ScIf(condition, consequent, alternative, _) =>
        val k = IfFrame(consequent, alternative, state)
        Set(EvalState(condition, env, state.push(k)))

      case ScLetRec(name, binding, body, _) => ???
      case ScRaise(error, idn) =>
        Set(RaiseError(error, idn.pos, state))

      case ScSet(variable, value, _) =>
        val k = SetFrame(variable, state)
        Set(EvalState(value, env, state.push(k)))
    }
    private def applyKont(value: Value, kont: ScFrame, rest: List[ScFrame]): Set[State] = ???
  }
}
