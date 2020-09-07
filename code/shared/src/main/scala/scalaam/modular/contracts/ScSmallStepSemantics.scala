package scalaam.modular.contracts
import scalaam.core.Environment
import scalaam.core.Position.Position
import scalaam.language.contracts.{
  ScBegin,
  ScExp,
  ScFunctionAp,
  ScIdentifier,
  ScIf,
  ScLetRec,
  ScNil,
  ScRaise,
  ScSet,
  ScValue
}
import scalaam.language.sexp.{ValueBoolean, ValueInteger}

trait ScSmallStepSemantics
    extends ScModSemantics
    with SmallstepSemantics[ScExp]
    with ScSymbolicComponents {

  override def intraAnalysis(component: Component): IntraScSmallStepSemantics
  trait IntraScSmallStepSemantics extends IntraScAnalysis with SmallstepSemanticsIntra {

    /**
      * Read from the given address in the cache, and if there is no such element in the cache
      * look it up from the store
      * @param addr
      * @return
      */
    def read(addr: Addr)(implicit cache: StoreCache): (Value, Sym) = cache.get(addr) match {
      case Some((value, sym)) => (value, sym)
      case None               => (readAddr(addr), ScNil())
    }

    /**
      * Writes both to the cache and the global store
      */
    def write(addr: Addr, value: Value, sym: Sym)(implicit cache: StoreCache): StoreCache = {
      writeAddr(addr, value)
      cache.get(addr) match {
        case Some((oldvalue, oldsym)) =>
          cache + (addr -> (lattice.join(oldvalue, value), oldsym.or(sym)))

        case None =>
          cache + (addr -> (value, sym))

      }
    }

    case class S(
        kont: ScFrame,
        env: Env = Environment(List()),
        cache: StoreCache = Map(),
        pc: PC = ScNil()
    )

    sealed trait ScState
    case class EvalState(exp: ScExp, env: Env, state: S)            extends ScState
    case class ApplyKont(value: Value, symbolic: Sym, state: S)     extends ScState
    case class RaiseError(message: String, pos: Position, state: S) extends ScState

    sealed trait ScFrame {
      def next: ScFrame
    }
    case class IfFrame(consequent: ScExp, alternative: ScExp, next: ScFrame) extends ScFrame
    case class LetrecFrame(ident: ScIdentifier, body: ScExp, env: Env, next: ScFrame)
        extends ScFrame
    case class SeqFrame(sequence: List[ScExp], next: ScFrame) extends ScFrame
    case class SetFrame(ident: ScIdentifier, next: ScFrame)   extends ScFrame

    case class EvalOperatorFrame(operands: List[ScExp], next: ScFrame) extends ScFrame
    case class EvalOperandsFrame(
        operator: PostValue,
        collected: List[PostValue],
        operands: List[ScExp],
        next: ScFrame
    ) extends ScFrame

    case class RestoreEnv(env: Env, next: ScFrame) extends ScFrame
    case object NilFrame extends ScFrame {
      def next: ScFrame = throw new Exception("No next frame for the nil frame")
    }

    type State = ScState
    override def step(state: State): Set[State] = state match {
      case EvalState(exp, env, state)                   => eval(exp, env, state)
      case ApplyKont(_, _, S(NilFrame, _, _, _))        => ??? // impossible
      case ApplyKont(value, sym, state @ S(k, _, _, _)) => applyKont(value, sym, k, state)
    }

    override def isFinalState(state: ScState): Boolean   = ???
    override def finalStateResult(state: ScState): Value = ???

    private def lookup(id: ScIdentifier, env: Env): Addr = env.lookup(id.name) match {
      case Some(addr) => addr
      case None       => throw new Exception(s"undefined variable ${id.name} at ${id.idn.pos}")
    }

    private def eval(exp: ScExp, env: Env, state: S): Set[State] = {
      implicit var cache: StoreCache = state.cache
      exp match {
        case ScBegin(expressions, _) =>
          val k = SeqFrame(expressions.tail, state.kont)
          Set(EvalState(expressions.head, env, state.copy(kont = k)))

        case ScIf(condition, consequent, alternative, _) =>
          val k = IfFrame(consequent, alternative, state.kont)
          Set(EvalState(condition, env, state.copy(kont = k)))

        case ScLetRec(ident, binding, body, _) =>
          val varAddr = allocVar(ident, component)
          val newEnv  = state.env.extend(ident.name, varAddr)
          val k       = LetrecFrame(ident, body, env, state.kont)
          Set(EvalState(binding, newEnv, state.copy(env = newEnv, kont = k)))

        case ScRaise(error, idn) =>
          Set(RaiseError(error, idn.pos, state.copy(env = env)))

        case ScSet(variable, value, _) =>
          val k = SetFrame(variable, state.kont)
          Set(EvalState(value, env, state.copy(kont = k)))

        case ScFunctionAp(operator, operands, _) =>
          val k = EvalOperatorFrame(operands, state.kont)
          Set(EvalState(operator, env, state.copy(kont = k)))

        case v @ ScValue(value, _) =>
          value match {
            case ValueInteger(i) =>
              Set(ApplyKont(lattice.injectInteger(i), v, state))
            case ValueBoolean(b) =>
              Set(ApplyKont(lattice.injectBoolean(b), v, state))
          }

        case exp: ScIdentifier =>
          val addr         = lookup(exp, env)
          val (value, sym) = read(addr)
          Set(ApplyKont(value, sym, state))

      }
    }
    private def applyKont(value: Value, sym: Sym, kont: ScFrame, state: S): Set[State] = {
      implicit var cache: StoreCache = state.cache
      kont match {
        case IfFrame(consequent, alternative, next) =>
          // TODO: use lattice to check whether the value could be true or not
          Set(
            EvalState(consequent, state.env, state.copy(pc = state.pc.and(sym), kont = next)),
            EvalState(alternative, state.env, state.copy(pc = state.pc.and(sym.not()), kont = next))
          )

        case RestoreEnv(env, next) =>
          Set(ApplyKont(value, sym, state.copy(env = env, kont = next)))

        case LetrecFrame(ident, body, oldEnv, next) =>
          val varAddr = lookup(ident, state.env)
          cache = write(varAddr, value, sym)
          val k        = RestoreEnv(oldEnv, next)
          val newState = state.copy(cache = cache, kont = k)
          Set(EvalState(body, state.env, newState))

        case SeqFrame(List(head), next) =>
          // last element in sequence
          Set(EvalState(head, state.env, state.copy(kont = next)))

        case SeqFrame(sequence, next) =>
          val k = SeqFrame(sequence.tail, next)
          Set(EvalState(sequence.head, state.env, state.copy(kont = k)))

        case SetFrame(ident, next) =>
          val addr = lookup(ident, state.env)
          cache = write(addr, value, sym)
          Set(ApplyKont(value, sym, state.copy(kont = next, cache = cache)))

        case EvalOperatorFrame(List(), _) =>
          applyOp(symbolic(value, sym), List(), state)

        case EvalOperatorFrame(operands, next) =>
          val k = EvalOperandsFrame(symbolic(value, sym), List(), operands.tail, next)
          Set(EvalState(operands.head, state.env, state.copy(kont = k)))

        case EvalOperandsFrame(operator, collected, List(), _) =>
          applyOp(operator, collected.reverse, state)

        case EvalOperandsFrame(operator, collected, operands, next) =>
          val k =
            EvalOperandsFrame(operator, symbolic(value, sym) :: collected, operands.tail, next)
          Set(EvalState(operands.head, state.env, state.copy(kont = k)))
      }
    }

    private def applyOp(operator: PostValue, operands: List[PostValue], state: S): Set[State] = {
      // we have two cases:
      // 1. a primitive is applied, in which case we can extend the symbolic expression
      // and run the operator directly
      // 2. a user-defined function is applied, in which case we need to register a read
      // dependency on the result of the function and add it to the work list, this is normally
      // handled by the `call` function

      // primitive function call
      val primitiveCall = lattice
        .getPrim(operator.value)
        .flatMap(
          prim =>
            Set(
              ApplyKont(
                lattice.applyPrimitive(prim)(operands.map(_.value): _*),
                operator.symbolic.app(operands.map(_.symbolic)),
                state.copy(kont = state.kont.next)
              )
            )
        )

      // user defined function
      val cloCall = lattice
        .getClo(operator.value)
        .flatMap(clo => {
          val context         = allocCtx(clo, operands.map(_.value), ???, component)
          val called          = Call(clo.env, clo.lambda, context)
          val calledComponent = newComponent(called)
          val result          = call(calledComponent)
          Set(ApplyKont(result, ScNil(), state.copy(kont = state.kont.next)))
        })

      primitiveCall ++ cloCall
    }
  }
}
