package scalaam.modular.contracts
import scalaam.core.{Address, Environment, Identity}
import scalaam.core.Position.Position
import scalaam.language.contracts.ScLattice.{Arr, Blame, Clo, Grd, Opq, Prim}
import scalaam.language.contracts.{ScBegin, ScDependentContract, ScExp, ScFlatContract, ScFunctionAp, ScHigherOrderContract, ScIdentifier, ScIf, ScLambda, ScLattice, ScLetRec, ScMon, ScNil, ScOpaque, ScRaise, ScSet, ScValue}
import scalaam.language.sexp.{ValueBoolean, ValueInteger}

trait ScSmallStepSemantics
    extends ScModSemantics
    with SmallstepSemantics[ScExp]
    with ScSymbolicComponents {

  type SMTSolver <: ScSmtSolver
  def newSmtSolver(program: ScExp): SMTSolver

  override def intraAnalysis(component: Component): IntraScSmallStepSemantics
  trait IntraScSmallStepSemantics extends IntraScAnalysis with SmallstepSemanticsIntra {
    private val primTrue  = ScLattice.Prim("true?")
    private val primFalse = ScLattice.Prim("false?")
    private val primProc  = ScLattice.Prim("proc?")
    private val primDep   = ScLattice.Prim("dependent-contract?")

    def writeBlame(blame: Blame) =
      writeAddr(ExceptionAddr(component, expr(component).idn), lattice.injectBlame(blame))

    def blame(
        state: S,
        blamedParty: Identity,
        blamingParty: Identity = Identity.none
    ): Set[State] = {
      val k = state.kont.last
      writeBlame(Blame(blamedParty, blamingParty))
      Set(ApplyKont(lattice.bottom, ScNil(), state.copy(kont = k)))
    }

    /**
      * Read from the given address in the cache, and if there is no such element in the cache
      * look it up from the store
      * @param addr
      * @return
      */
    def read(addr: Addr)(implicit cache: StoreCache): (Value, Sym) = cache.get(addr) match {
      case Some((value, sym)) => (value, sym)
      case None => {
        (readAddr(addr), ScNil())
      }
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
    case class ReturnResult(value: Value, state: S)                 extends State
    case class RaiseError(message: String, pos: Position, state: S) extends ScState

    sealed trait ScFrame {
      def next: ScFrame
      def last: ScFrame = this.next match {
        case ScNilFrame => this
        case _          => this.next.last
      }
    }
    case object ScNilFrame extends ScFrame {
      def next: ScFrame = throw new Exception("at the bottom of the continuation stack")
    }

    case class IfFrame(consequent: ScExp, alternative: ScExp, next: ScFrame) extends ScFrame
    case class LetrecFrame(ident: ScIdentifier, body: ScExp, env: Env, next: ScFrame)
        extends ScFrame
    case class SeqFrame(sequence: List[ScExp], next: ScFrame) extends ScFrame
    case class SetFrame(ident: ScIdentifier, next: ScFrame)   extends ScFrame
    case class EvalMonFrame(exp: ScExp, next: ScFrame)        extends ScFrame
    case class FinishEvalMonFrame(contract: Value, expressionIdn: Identity, next: ScFrame)
        extends ScFrame

    case class EvalRangeMakerFrame(exp: ScExp, next: ScFrame) extends ScFrame
    case class CheckRangeFrame(value: Value, sym: Sym, serverIdentity: Identity, next: ScFrame)
        extends ScFrame
    case class CheckedRangeFrame(value: Value, sym: Sym, serverIdentity: Identity, next: ScFrame)
        extends ScFrame

    case class AppliedRangeMakerFrame(
        value: Value,
        value1: List[PostValue],
        serverIdentity: Identity,
        next: ScFrame
    ) extends ScFrame

    case class ApplyRangeMakerFrame(
        rangeMaker: Value,
        e: Value,
        operands: List[PostValue],
        serverIdentity: Identity,
        next: ScFrame
    ) extends ScFrame

    case class FinishEvalDependentFrame(
        value: ScGenericAddr[AllocationContext],
        next: ScFrame
    ) extends ScFrame

    case class EvalOperatorFrame(operands: List[ScExp], next: ScFrame) extends ScFrame
    case class EvalOperandsFrame(
        operator: PostValue,
        collected: List[PostValue],
        operands: List[ScExp],
        next: ScFrame
    ) extends ScFrame

    case class RestoreEnv(env: Env, next: ScFrame) extends ScFrame
    case class ReturnFrame(next: ScFrame)          extends ScFrame
    case object NilFrame extends ScFrame {
      def next: ScFrame = throw new Exception("No next frame for the nil frame")
    }

    type State = ScState
    override def step(state: State): Set[State] = state match {
      case EvalState(exp, env, state)                   => eval(exp, env, state)
      case ApplyKont(_, _, S(NilFrame, _, _, _))        => ??? // impossible
      case ApplyKont(value, sym, state @ S(k, _, _, _)) => applyKont(value, sym, k, state)
    }

    override def isFinalState(state: ScState): Boolean =
      state.isInstanceOf[ReturnResult]

    override def finalStateResult(state: ScState): Value = state match {
      case ReturnResult(value, _) => value
    }

    override def initialState: ScState = {
      EvalState(fnBody, fnEnv, S(kont = ReturnFrame(ScNilFrame), env = fnEnv))
    }

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

        case ScMon(contract, expression, _) =>
          // first eval the contract, depending on the contract the value of mon changes
          val k = EvalMonFrame(expression, state.kont)
          Set(EvalState(contract, env, state.copy(kont = k)))

        case ScOpaque(idn) =>
          Set(ApplyKont(lattice.injectOpq(Opq()), ScNil(), state))

        case ScHigherOrderContract(domain, range, idn) =>
          // a higher order contract is evaluated the same as a dependent contract
          // except that we need to transform the range into range maker, we can to this by thunkifying the range
          val thunk = ScLambda(List(ScIdentifier("\"_", Identity.none)), range, range.idn)
          Set(EvalState(ScDependentContract(domain, thunk, idn), env, state))

        case ScDependentContract(domain, rangeMaker, _) =>
          val k = EvalRangeMakerFrame(rangeMaker, state.kont)
          Set(EvalState(domain, env, state.copy(kont = k)))

        case ScFlatContract(contract, _) =>
          Set(EvalState(contract, env, state))

        case lambda@ScLambda(params, _, idn) =>
          Set(ApplyKont(lattice.injectClo(Clo(idn, env, params, lambda)), ScNil(), state))
      }
    }

    private def applyKont(value: Value, sym: Sym, kont: ScFrame, state: S): Set[State] = {
      implicit var cache: StoreCache = state.cache
      kont match {
        case IfFrame(consequent, alternative, next) =>
          conditional(
            value,
            sym,
            state.pc,
            pNext => Set(EvalState(consequent, state.env, state.copy(pc = pNext, kont = next))),
            pNext => Set(EvalState(alternative, state.env, state.copy(pc = pNext, kont = next)))
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
          applyOp(operator, (symbolic(value, sym) :: collected).reverse, state)

        case EvalOperandsFrame(operator, collected, operands, next) =>
          val k =
            EvalOperandsFrame(operator, symbolic(value, sym) :: collected, operands.tail, next)
          Set(EvalState(operands.head, state.env, state.copy(kont = k)))

        case EvalRangeMakerFrame(rangeMaker, next) =>
          val addr = allocGeneric(component)
          cache = write(addr, value, sym)
          val k = FinishEvalDependentFrame(addr, next)
          Set(EvalState(rangeMaker, state.env, state.copy(kont = k, cache = cache)))

        case ApplyRangeMakerFrame(rangeMaker, e, operands, serverIdentity, next) =>
          // the domain has been evaluated
          conditional(
            value,
            sym,
            state.pc,
            pNext => {
              val k = AppliedRangeMakerFrame(e, operands, serverIdentity, next)
              applyOp(PostValue(rangeMaker, ScNil()), operands, state.copy(kont = k, pc = pNext))
            },
            // Example:
            // ((mon (int? ~ (lambda (x) int?)) (lambda (x) ...)) OPQ) blame should be on the location of OPQ
            pNext => ??? // generate blame on the caller
          )

        case AppliedRangeMakerFrame(e, operands, serverIdentity, next) =>
          // TODO: check if range is a proc?, otherwise generate blame
          val k = CheckRangeFrame(value, sym, serverIdentity, next)
          applyOp(PostValue(e, ScNil()), operands, state.copy(kont = k))

        case CheckRangeFrame(range, symRange, serverIdentity, next) =>
          val k = CheckedRangeFrame(value, sym, serverIdentity, next)
          applyOp(PostValue(range, symRange), List(PostValue(value, sym)), state.copy(kont = k))

        case CheckedRangeFrame(value, sym, serverIdentity, next) =>
          // if the output of the applied function does not satisfy the contract we will generate a blame
          // on the applied function
          conditional(
            value,
            sym,
            state.pc,
            pNext => Set(ApplyKont(value, sym, state.copy(kont = next, pc = pNext))),
            pNext =>
              blame(state.copy(kont = next, pc = pNext), serverIdentity) // generate blame on the lambda
          )

        case FinishEvalDependentFrame(domainAddr, next) =>
          val rangeAddr = allocGeneric(component)
          cache = write(rangeAddr, value, sym)
          Set(
            ApplyKont(
              lattice.injectGrd(Grd(domainAddr, rangeAddr)),
              ScNil(),
              state.copy(kont = kont.next, cache = cache)
            )
          )

        case EvalMonFrame(expr, next) =>
          val k = FinishEvalMonFrame(value, expr.idn, next)
          Set(EvalState(expr, state.env, state.copy(kont = k)))

        case FinishEvalMonFrame(contract, idn, _) =>
          mon(contract, value, sym, idn, state)

        case ReturnFrame(_) =>
          Set(ReturnResult(value, state))
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
      val primitiveCall = applyOpPrim(operator, operands).flatMap(
        value => {
          Set(
            ApplyKont(
              value,
              operator.symbolic.app(operands.map(_.symbolic)),
              state.copy(kont = state.kont.next)
            )
          )
        }
      )

      // user defined function
      val cloCall = applyOpClo(operator, operands).flatMap(
        result => Set(ApplyKont(result, ScNil(), state.copy(kont = state.kont.next)))
      )

      // monitored function
      // a monitored function first applies its domain contract to the input value of the function (operands)
      // then it passes the input to the range maker, finally, it executes the function and afterwards, it
      // evaluates the range contract
      val arrCall = lattice
        .getArr(operator.value)
        .flatMap(arr => {
          //        domain  range e               operands
          // ((mon (int? -> any?) (lambda (x) _)) OPQ)
          val grds   = read(arr.contract)(state.cache)
          val (e, _) = read(arr.e)(state.cache)
          lattice
            .getGrd(grds._1)
            .flatMap(grd => {
              val (domain, _)     = read(grd.domain)(state.cache)
              val (rangeMaker, _) = read(grd.rangeMaker)(state.cache)
              val k               = ApplyRangeMakerFrame(rangeMaker, e, operands, arr.lserver, state.kont.next)
              applyOp(PostValue(domain, ScNil()), operands, state.copy(kont = k))
            })
        })

      primitiveCall ++ cloCall ++ arrCall
    }

    private def applyOpPrim(
        operator: PostValue,
        operands: List[PostValue]
    ): Set[Value] = {
      lattice
        .getPrim(operator.value)
        .map(lattice.applyPrimitive(_)(operands.map(_.value): _*))
    }

    private def applyOpClo(
        operator: PostValue,
        operands: List[PostValue],
    ): Set[Value] = {
      // user defined function
      lattice
        .getClo(operator.value)
        .map(clo => {
          val context         = allocCtx(clo, operands.map(_.value), clo.lambda.idn.pos, component)
          val called          = Call(clo.env, clo.lambda, context)
          val calledComponent = newComponent(called)
          operands.zip(clo.lambda.variables.map(v => allocVar(v, calledComponent))).foreach {
            case (value, addr) => writeAddr(addr, value.value)
          }

          val result          = call(calledComponent)
          result
        })
    }

    private def applyOpResult(operator: PostValue, operands: List[PostValue]): Set[Value] =
      applyOpPrim(operator, operands) ++ applyOpClo(operator, operands)

    /**
      * Creates a monitor on the given value.
      * (mon contract/Identity.none expr/idn)
      */
    private def mon(
        contract: Value,
        expr: Value,
        sym: Sym,
        eIdn: Identity,
        state: S
    ): Set[State] = {
      // we either have a clojure/primitive
      val flatContract: Set[State] = feasible(primProc, state.pc, contract, ScNil()).toSet.flatMap {
        _: PC =>
          val result =
            lattice.join(applyOpResult(PostValue(contract, ScNil()), List(PostValue(expr, sym))))
          conditional(
            result,
            ScNil(),
            state.pc,
            pNext => Set(ApplyKont(expr, sym, state.copy(kont = state.kont.next, pc = pNext))),
            pNext => blame(state.copy(kont = state.kont.next, pc = pNext), eIdn)
          )
      }

      val dependentContract: Set[State] =
        feasible(primDep, state.pc, contract, ScNil()).toSet.flatMap { _: PC =>
          val valueAddr = allocGeneric(component)
          val grdAddr   = allocGeneric(component)
          var cache     = write(valueAddr, expr, sym)(state.cache)
          cache = write(grdAddr, contract, ScNil())(cache)
          Set(
            ApplyKont(
              lattice.injectArr(Arr(Identity.none, sym.idn, grdAddr, valueAddr)),
              ScNil(),
              state.copy(kont = state.kont.next, cache = cache)
            )
          )
        }

      val all = flatContract ++ dependentContract

      // if `contract` was not a valid contract, then we generate a blame
      if (all.isEmpty) ??? else all
    }

    /**
      * Generates new states for the result of a conditional
      * @param value the value of the condition
      * @param sym the symbolic value of the condition
      * @param pc the path condition upto the conditional
      * @param t a lambda that generates new states based on the updated path condition for the true branch
      * @param f a lambda that generates new states based on the updated path condition for the false branch
      * @return a set of new states to execute next
      */
    private def conditional(
        value: Value,
        sym: Sym,
        pc: PC,
        t: PC => Set[State],
        f: PC => Set[State]
    ): Set[State] =
      // a conditional consists of a true branch and a false branch, we use the built-in predicates
      // true? and false? to check if the true and false branch are feasible
      feasible(primTrue, pc, value, sym).toSet.flatMap(t) ++
        feasible(primFalse, pc, value, sym).toSet.flatMap(f)

    /**
      * Checks wether the given operation is feasible for the given path with the given value
      * If it is it returns the path condition otherwise it returns none
      */
    private def feasible(op: Prim, pc: PC, value: Value, sym: Sym): Option[PC] = {
      sym match {
        // if operation applied on the value is not true, then the path is infeasible
        case _ if !lattice.isTrue(lattice.applyPrimitive(op)(value)) => None
        // we are not going to extend the path condition if we do not have any symbolic information
        case ScNil(_) => Some(pc)
        // otherwise we let the SMT solver decide whether the path is feasible
        case _ =>
          val solver = newSmtSolver(pc.and(sym))
          if (solver.isSat) Some(pc.and(sym)) else None
      }
    }
  }
}