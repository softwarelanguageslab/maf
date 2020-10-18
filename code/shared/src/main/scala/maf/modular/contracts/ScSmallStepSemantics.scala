package maf.modular.contracts
import maf.core.{Address, Environment, Identity}
import maf.core.Position.Position
import maf.language.contracts.ScLattice.{Arr, Blame, Clo, Flat, Grd, Opq, Prim}
import maf.language.contracts.{
  ScBegin,
  ScCheck,
  ScDependentContract,
  ScExp,
  ScFlatContract,
  ScFunctionAp,
  ScHigherOrderContract,
  ScIdentifier,
  ScIf,
  ScLambda,
  ScLattice,
  ScLetRec,
  ScMon,
  ScNil,
  ScOpaque,
  ScRaise,
  ScSet,
  ScValue
}
import maf.language.sexp.{ValueBoolean, ValueInteger}
import maf.util.{Monoid, SingletonSet}

import scala.util.Random
import akka.japi.pf.FI.Apply

object ScSmallStepSemantics {
  var r = 0
  def genSym: String = {
    r += 1
    s"x$r"
  }
}

trait ScSmallStepSemantics
    extends ScModSemantics
    with SmallstepSemantics[ScExp]
    with ScPrimitives
    with ScSymbolicComponents {

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
      state.blaming.headOption match {
        case Some(BlamingContext(blamed, _)) =>
          writeBlame(Blame(blamed, blamingParty))
        case _ =>
          writeBlame(Blame(blamedParty, blamingParty))
      }
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

    case class BlamingContext(blamed: Identity, monitor: Identity)

    case class S(
        kont: ScFrame,
        env: Env = Environment(List()),
        cache: StoreCache = Map(),
        pc: PC = ScNil(),
        blaming: List[BlamingContext] = List(),
        appIdn: Option[Identity] = None,
        flatIdn: Option[Identity] = None
    ) {

      /**
        * Get the current blaming context
        */
      def blamingContext: BlamingContext = blaming.head

      /**
        * Pop the blaming context from the context stack
        */
      def popBlamingContext: S =
        this.copy(blaming = blaming.tail)

      /**
        * Add a new blaming context to the context stack
        */
      def pushBlamingContext(blamingContext: BlamingContext): S =
        this.copy(blaming = blamingContext :: blaming)

      /**
        * Replace the current element of the blaming stack
        */
      def replaceBlamingContext(blamingContext: BlamingContext): S =
        this.copy(blaming = blamingContext :: blaming.tail)
    }

    object FrameS {
      def unapply(m: Any): Option[(ScFrame)] = m match {
        case m: S => Some(m.kont)
        case _    => None
      }
    }

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

    case class CheckFrame(exp: ScExp, next: ScFrame)  extends ScFrame
    case class FlatContractResultFrame(next: ScFrame) extends ScFrame
    case class PopBlameContext(next: ScFrame)         extends ScFrame

    case class IfFrame(consequent: ScExp, alternative: ScExp, next: ScFrame) extends ScFrame
    case class LetrecFrame(ident: ScIdentifier, body: ScExp, env: Env, next: ScFrame)
        extends ScFrame
    case class SeqFrame(sequence: List[ScExp], next: ScFrame) extends ScFrame
    case class SetFrame(ident: ScIdentifier, next: ScFrame)   extends ScFrame
    case class EvalMonFrame(exp: ScExp, contractIdn: Identity, monitorIdn: Identity, next: ScFrame)
        extends ScFrame
    case class FinishEvalMonFrame(
        contract: Value,
        contractIdn: Identity,
        expressionIdn: Identity,
        monitorIdn: Identity,
        next: ScFrame
    ) extends ScFrame

    case class EvalRangeMakerFrame(exp: ScExp, domainIdn: Identity, next: ScFrame) extends ScFrame
    case class CheckRangeFrame(value: Value, sym: Sym, serverIdentity: Identity, next: ScFrame)
        extends ScFrame
    case class CheckedRangeFrame(value: Value, sym: Sym, serverIdentity: Identity, next: ScFrame)
        extends ScFrame

    case class RangeMakerResultFrame(
        value: Value,
        value1: List[PostValue],
        serverIdentity: Identity,
        next: ScFrame
    ) extends ScFrame

    case class ApplyRangeMakerFrame(
        rangeMaker: Value,
        e: Value,
        operands: List[PostValue],
        callerIdentity: Identity,
        serverIdentity: Identity,
        next: ScFrame
    ) extends ScFrame

    case class EvalFlatContractFrame(contractIdn: Identity, next: ScFrame) extends ScFrame

    case class FinishEvalDependentFrame(
        value: ScGenericAddr[AllocationContext],
        rangeMakerIdentity: Identity,
        next: ScFrame
    ) extends ScFrame

    case class EvalOperatorFrame(operands: List[ScExp], next: ScFrame) extends ScFrame
    case class EvalOperandsFrame(
        operator: PostValue,
        currentExp: ScExp,
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
      case EvalState(exp, env, state)               => eval(exp, env, state)
      case ApplyKont(_, _, FrameS(NilFrame))        => ??? // impossible
      case ApplyKont(value, sym, state @ FrameS(k)) => applyKont(value, sym, k, state)
    }

    override def isFinalState(state: ScState): Boolean =
      state.isInstanceOf[ReturnResult]

    override def finalStateResult(state: ScState): Value = state match {
      case ReturnResult(value, _) => value
    }

    override def initialState: ScState = {
      var cache: Map[Addr, (Value, ScExp)] = Map()
      primBindings.foreach {
        case (name, addr) =>
          cache += addr -> (
            lattice.injectPrim(Prim(name)),
            ScIdentifier(name, Identity.none)
          )
      }

      fnEnv.content.values.foreach { addr =>
        val value = readAddr(addr)
        if (lattice.isDefinitelyOpq(value)) {
          cache += addr -> (value, ScIdentifier(ScSmallStepSemantics.genSym, Identity.none))
        }
      }

      val context = component match {
        case ContractCall(mon, blamed, _, _, _) =>
          List(BlamingContext(blamed, mon))

        case _ => List()
      }

      EvalState(
        fnBody,
        fnEnv,
        S(
          kont = ReturnFrame(ScNilFrame),
          env = fnEnv,
          cache = cache,
          blaming = context
        )
      )
    }

    private def lookup(id: ScIdentifier, env: Env): Addr = env.lookup(id.name) match {
      case Some(addr) => addr
      case None       => throw new Exception(s"undefined variable ${id.name} at ${id.idn.pos}")
    }

    private def eval(exp: ScExp, env: Env, state: S): Set[State] = {
      implicit val cache: StoreCache = state.cache
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

        case ScFunctionAp(operator, operands, idn) =>
          val k = EvalOperatorFrame(operands, state.kont)
          Set(EvalState(operator, env, state.copy(kont = k, appIdn = Some(idn))))

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

        case ScMon(contract, expression, idn) =>
          // (mon contract expression)/idn
          val k = EvalMonFrame(expression, contract.idn, idn, state.kont)
          Set(EvalState(contract, env, state.copy(kont = k)))

        case ScOpaque(_, refinements) =>
          // (OPQ) | (OPQ refinement)
          Set(ApplyKont(lattice.injectOpq(Opq(refinements)), ScNil(), state))

        case ScHigherOrderContract(domain, range, idn) =>
          // (~> domain range) => (~> domain (lambda (_) range))
          val thunk = ScLambda(List(ScIdentifier("\"_", range.idn)), range, range.idn)
          Set(EvalState(ScDependentContract(domain, thunk, idn), env, state))

        case ScDependentContract(domain, rangeMaker, _) =>
          // (~ domain rangeMaker)
          val k = EvalRangeMakerFrame(rangeMaker, domain.idn, state.kont)
          Set(EvalState(domain, env, state.copy(kont = k)))

        case ScFlatContract(expression, _) =>
          // (flat expression)
          val k = EvalFlatContractFrame(exp.idn, state.kont)
          Set(EvalState(expression, env, state.copy(kont = k)))

        case lambda @ ScLambda(params, _, idn) =>
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

        case EvalFlatContractFrame(flatContractIdn, next) =>
          val contractAddr = allocGeneric(flatContractIdn, component)
          val cache        = write(contractAddr, value, sym)(state.cache)
          Set(
            ApplyKont(
              lattice.injectFlat(Flat(contractAddr)),
              sym,
              state.copy(kont = next, cache = cache)
            )
          )

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

        case EvalOperatorFrame(List(), next) =>
          applyOp(symbolic(value, sym), List(), state.copy(kont = next))

        case EvalOperatorFrame(operands, next) =>
          val k =
            EvalOperandsFrame(symbolic(value, sym), operands.head, List(), operands.tail, next)
          Set(EvalState(operands.head, state.env, state.copy(kont = k)))

        case EvalOperandsFrame(operator, prevExp, collected, List(), next) =>
          applyOp(
            operator,
            (symbolic(value, sym, prevExp.idn) :: collected).reverse,
            state.copy(kont = next)
          )

        case EvalOperandsFrame(operator, prevExp, collected, operands, next) =>
          val k =
            EvalOperandsFrame(
              operator,
              operands.head,
              symbolic(value, sym, prevExp.idn) :: collected,
              operands.tail,
              next
            )
          Set(EvalState(operands.head, state.env, state.copy(kont = k)))

        case EvalRangeMakerFrame(rangeMaker, domainIdn, next) =>
          val addr = allocGeneric(domainIdn, component)
          cache = write(addr, value, sym)
          val k = FinishEvalDependentFrame(addr, rangeMaker.idn, next)
          Set(EvalState(rangeMaker, state.env, state.copy(kont = k, cache = cache)))

        case ApplyRangeMakerFrame(rangeMaker, e, operands, callerIdentity, serverIdentity, next) =>
          val k = RangeMakerResultFrame(e, List(symbolic(value, sym)), serverIdentity, next)
          applyOp(PostValue(rangeMaker, ScNil()), List(symbolic(value, sym)), state.copy(kont = k))

        case RangeMakerResultFrame(e, operands, serverIdentity, next) =>
          // TODO: check if range is a proc?, otherwise generate blame
          val k = CheckRangeFrame(value, sym, serverIdentity, next)
          applyOp(PostValue(e, ScNil()), operands, state.copy(kont = k))

        case CheckRangeFrame(range, symRange, serverIdentity, next) =>
          val k = PopBlameContext(CheckedRangeFrame(value, sym, serverIdentity, next))
          applyOp(
            PostValue(range, symRange),
            List(PostValue(value, sym)),
            // TODO: this can be more precise, as serverIdentity/serverIdentity is not sensitive to the location
            // where the function is applied
            state.copy(kont = k).pushBlamingContext(BlamingContext(serverIdentity, serverIdentity))
          )

        case CheckedRangeFrame(result, sym, serverIdentity, next) =>
          // if the output of the applied function does not satisfy the contract we will generate a blame
          // on the applied function
          conditional(
            value,
            sym,
            state.pc,
            pNext => Set(ApplyKont(result, sym, state.copy(kont = next, pc = pNext))),
            pNext =>
              blame(state.copy(kont = next, pc = pNext), serverIdentity) // generate blame on the lambda
          )

        case FinishEvalDependentFrame(domainAddr, rangeIdn, next) =>
          val rangeAddr = allocGeneric(rangeIdn, component)
          cache = write(rangeAddr, value, sym)
          Set(
            ApplyKont(
              lattice.injectGrd(Grd(domainAddr, rangeAddr)),
              ScNil(),
              state.copy(kont = next, cache = cache)
            )
          )

        case EvalMonFrame(expr, contractIdn, monitorIdn, next) =>
          val k = FinishEvalMonFrame(value, contractIdn, expr.idn, monitorIdn, next)
          Set(EvalState(expr, state.env, state.copy(kont = k)))

        case FinishEvalMonFrame(contract, contractIdn, expressionIdn, monitorIdn, _) =>
          mon(contract, value, sym, contractIdn, expressionIdn, monitorIdn, state)

        case PopBlameContext(next) =>
          Set(ApplyKont(value, sym, state.copy(kont = next).popBlamingContext))

        case FlatContractResultFrame(next) =>
          val monIdn  = state.blaming.headOption.map(_.monitor)
          val appIdn  = state.appIdn
          val flatIdn = state.flatIdn

          conditional(
            value,
            sym,
            state.pc,
            (pNext) => {
              monIdn.zip(appIdn.zip(flatIdn)).foreach {
                case (monIdn, verify) => addVerified(monIdn, verify)
              }
              Set(ApplyKont(value, sym, state.copy(pc = pNext, kont = next)))
            },
            (pNext) => {
              monIdn.zip(appIdn.zip(flatIdn)).foreach {
                case (monIdn, verify) => addUnverified(monIdn, verify)
              }
              Set(ApplyKont(value, sym, state.copy(pc = pNext, kont = next)))
            }
          )

        case ReturnFrame(_) =>
          Set(ReturnResult(value, state))
      }
    }

    private def contextSensitiveCall(
        clo: Clo[Addr],
        operands: List[Value],
        createComponent: CreateCallComponent = Call.apply
    ): Value = {
      val context         = allocCtx(clo, operands, clo.lambda.idn.pos, component)
      val called          = createComponent(clo.env, clo.lambda, context)
      val calledComponent = newComponent(called)
      operands.zip(clo.lambda.variables.map(v => allocVar(v, calledComponent))).foreach {
        case (value, addr) => {
          writeAddr(addr, value)
        }
      }

      val result = call(calledComponent)
      result
    }

    private def applyPrimOp[A](
        operator: PostValue,
        operands: List[PostValue]
    )(f: (Value => Set[A])): Set[A] = {
      lattice.getPrim(operator.value).flatMap { prim =>
        f(lattice.applyPrimitive(prim)(operands.map(_.value): _*))
      }
    }

    private def applyClo[A](
        operator: PostValue,
        operands: List[PostValue],
        createComponent: CreateCallComponent = Call.apply
    )(f: (Value => Set[A])): Set[A] = {
      lattice.getClo(operator.value).flatMap { clo =>
        f(contextSensitiveCall(clo, operands.map(_.value), createComponent))
      }
    }

    private def applyFlat[A](
        operator: PostValue,
        operands: List[PostValue],
        createComponent: CreateCallComponent = Call.apply
    )(f: (Value => Set[A])): Set[A] = {
      lattice.getFlat(operator.value).flatMap { flat =>
        val wrappedOperator = readAddr(flat.contract)
        val result = applyOpResult(PostValue(wrappedOperator, ScNil()), operands, createComponent)
          .foldLeft(lattice.bottom)((v, value) => lattice.join(v, value))

        f(result)
      }
    }

    private def applyOp(operator: PostValue, operands: List[PostValue], state: S): Set[State] = {
      // primitive function call
      val primitiveCall = applyPrimOp(operator, operands) { value =>
        Set(
          ApplyKont(
            value,
            operator.symbolic.app(operands.map(_.symbolic)),
            state
          )
        )
      }

      // user defined function
      val cloCall = applyClo(operator, operands, makeContractCall(state)) { result =>
        Set(ApplyKont(result, ScNil(), state))
      }

      // application of a flat contract
      val flatCall = lattice.getFlat(operator.value).flatMap { flat =>
        val (value, sym) =
          read(flat.contract)(state.cache)
        val k = FlatContractResultFrame(state.kont)

        applyOp(
          PostValue(value, sym),
          operands,
          state.copy(kont = k, flatIdn = Some(flat.contract.idn))
        )
      }

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
              val k = PopBlameContext(
                ApplyRangeMakerFrame(
                  rangeMaker,
                  e,
                  operands,
                  operands.head.idn,
                  arr.lserver,
                  state.kont
                )
              )

              if (operands.length == 1) {
                monFlat(
                  domain,
                  operands.head.value,
                  operands.head.symbolic,
                  operands.head.idn,
                  state
                    .copy(kont = k)
                    .pushBlamingContext(BlamingContext(operands.head.idn, state.appIdn.get))
                )
              } else {
                throw new Exception("Only domains with one operand are currently allowed")
              }
            })
        })

      primitiveCall ++ cloCall ++ arrCall ++ flatCall
    }

    private def applyOpResult(
        operator: PostValue,
        operands: List[PostValue],
        createComponent: CreateCallComponent = Call.apply
    ): Set[Value] =
      applyPrimOp(operator, operands)(SingletonSet.apply) ++
        applyClo(operator, operands, createComponent)(SingletonSet.apply) ++
        applyFlat(operator, operands, createComponent)(SingletonSet.apply)

    def makeContractCall(
        state: S
    )(env: Env, lambda: ScLambda, context: ComponentContext): Call[ComponentContext] =
      state.blaming.headOption match {
        case Some(BlamingContext(blamedParty, mon)) =>
          ContractCall(mon, blamedParty, env, lambda, context)
        case _ => Call(env, lambda, context)
      }

    def enrich(value: Value, contract: Value): Value =
      if (lattice.isDefinitelyOpq(value)) {
        val opqs = lattice
          .getOpq(value)
          .map((opq) => Opq(opq.refinementSet ++ lattice.getPrim(contract).map(_.operation)))
          .map(lattice.injectOpq)

        opqs.foldLeft(lattice.bottom)((v, opq) => lattice.join(v, opq))
      } else {
        value
      }

    def monFlat(contract: Value, expr: Value, sym: Sym, eIdn: Identity, state: S): Set[State] = {
      feasible(primProc, state.pc, contract, ScNil()).toSet.flatMap { _: PC =>
        val result =
          lattice.join(
            applyOpResult(
              PostValue(contract, ScNil()),
              List(PostValue(expr, sym)),
              makeContractCall(state)
            )
          )

        if (lattice.bottom == result) {
          Set(ApplyKont(expr, sym, state))
        } else {
          conditional(
            result,
            ScNil(),
            state.pc,
            pNext =>
              Set(
                ApplyKont(enrich(expr, contract), sym, state.copy(kont = state.kont, pc = pNext))
              ),
            pNext => blame(state.copy(kont = state.kont, pc = pNext), eIdn)
          )
        }
      }
    }

    /**
      * Creates a monitor on the given value.
      * (mon contract/cIdn expr/idn)/mIdn
      */
    private def mon(
        contract: Value,
        expr: Value,
        sym: Sym,
        cIdn: Identity,
        eIdn: Identity,
        mIdn: Identity,
        state: S
    ): Set[State] = {
      // we either have a clojure/primitive
      val flatContract = monFlat(
        contract,
        expr,
        sym,
        eIdn,
        state
          .copy(kont = PopBlameContext(state.kont.next))
          .pushBlamingContext(BlamingContext(eIdn, mIdn))
      )

      val dependentContract: Set[State] =
        feasible(primDep, state.pc, contract, ScNil()).toSet.flatMap { _: PC =>
          val valueAddr = allocGeneric(eIdn, component)
          val grdAddr   = allocGeneric(cIdn, component)
          var cache     = write(valueAddr, expr, sym)(state.cache)
          cache = write(grdAddr, contract, ScNil())(cache)
          Set(
            ApplyKont(
              lattice.injectArr(Arr(cIdn, eIdn, grdAddr, valueAddr)),
              ScNil(),
              state.copy(kont = state.kont.next, cache = cache)
            )
          )
        }

      val all = flatContract ++ dependentContract

      // if `contract` was not a valid contract, then we generate a blame
      if (all.isEmpty) {
        if (contract == lattice.bottom) {
          Set(ApplyKont(expr, sym, state.copy(kont = state.kont.next)))
        } else {
          ??? // invalid contract generate a blame
        }
      } else all
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

          val isSat = solver.isSat
          if (isSat) Some(pc.and(sym)) else None
      }
    }
  }
}
