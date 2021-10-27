package maf.aam

import maf.modular.ModAnalysis

import maf.modular.scheme._
import maf.core.Position._
import maf.core._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.util.benchmarks.Timeout
import maf.language.sexp
import maf.language.CScheme._
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.LatticeWithAddrs

/** An AAM style semantics for Scheme */
abstract class SchemeAAMSemantics(prog: SchemeExp) extends AAMAnalysis with SchemeDomainAAM:
    type Val = Value
    type Expr = SchemeExp
    type Kont = Frame
    type Sto = BasicStore[Address, Storable]
    type Lam = SchemeLambdaExp
    type State = SchemeState
    type Env = Environment[Address]
    type Ctx

    /**
     * The `Storable` enum represents values that can be stored in the store. To enable approximating semantics we define a lattice over these
     * storable values below.
     */
    enum Storable:
        /** We can store values in the store */
        case V(v: Val)

        /** We can also store continuations in the store */
        case K(k: Set[Kont])

    /** Instantiation of the `Storable` lattice. Only elements of the same type can be joined together, and there are no bottom or top elements */
    given storableLattice: Lattice[Storable] =
        import Storable.*
        def bottom: Storable = throw new Exception("storables have no single bottom element")
        def top: Storable = throw new Exception("storables have no single top element")
        def join(x: Storable, y: Storable): Storable = (x, y) match
            case (V(v1), V(v2)) => V(lattice.join(v1, v2))
            case (K(k1), K(k2)) => K(k1 ++ k2)
            case _              => throw new Exception("trying to join incompatible elements")

    /**
     * An address under which a continuation is stored. To preserve call-return semantics, this address should be (e, p) where e is the call targets
     * control and environment respectively (Gilray et al., 2016).
     */
    case class KontAddr(e: Expr, rho: Env) extends Address:
        def idn: Identity = e.idn
        def printable = true
        override def toString = s"KontAddr(${e}, ${rho})"

    /** The location of the initial continuaton */
    case object Kont0Addr extends Address

    /** An address on which values will be allocated */
    case class ValueAddr(lam: Lam, ctx: Ctx) extends Address:
        def idn: Identity = lam.idn
        def printable = true
        override def toString = s"ValueAddr(${lam}, ${ctx})"

    /** Read from the given address in the store, returns V(bottom) if no value is found at the given address. */
    def readSto(sto: Sto, addr: Address): Storable =
      sto.lookup(addr).getOrElse(Storable.V(lattice.bottom))

    /** Write to the given address in the store, returns the updated store */
    def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.update(addr, value)

    sealed trait Frame:
        def link(next: Address): Frame
    case class AssFrame(id: Identifier, next: Option[Address] = None) extends Frame:
        def link(next: Address): AssFrame =
          AssFrame(id, Some(next))
    case class BegFrame(exps: List[Expr], next: Option[Address] = None) extends Frame:
        def link(next: Address): BegFrame =
          this.copy(next = Some(next))
    case class IteFrame(csq: Expr, alt: Expr, next: Option[Address] = None) extends Frame:
        def link(next: Address): IteFrame =
          this.copy(next = Some(next))
    case class RetFrame(env: Env, next: Option[Address] = None) extends Frame:
        def link(next: Address): RetFrame =
          this.copy(next = Some(next))

    /**
     * A continuation for evaluating a function.
     *
     * @param f
     *   the original Scheme expression corresponding to the function call
     * @param args
     *   the list of arguments that still need to be evaluated
     */
    case class FunFrame(f: SchemeFuncall, args: List[SchemeExp], next: Option[Address] = None) extends Frame:
        def link(next: Address): FunFrame =
          this.copy(next = Some(next))

    case class ArgFrame(f: SchemeFuncall, args: List[SchemeExp], fv: Value, argV: List[Value], next: Option[Address] = None) extends Frame:
        def link(next: Address): ArgFrame =
          this.copy(next = Some(next))

    /** The control of the CESK* machine */
    enum Control:
        /** Instruction to evaluate the given expression */
        case Ev(expr: Expr)

        /** Instruction to apply the continuation that is on top of the continuation stack */
        case Ap(value: Value)

    /** A state of the Scheme AAM machine */
    case class SchemeState(c: Control, e: Env, s: Sto, k: Address)

    /** Inject the initial state for the given expression */
    def inject(expr: Expr): State =
        val initialEnv = ??? // TODO
        val initialStore = ??? // BasicStore.empty.extend(Kont0Addr, Storable.K(HltFrame)) // TODO: add initial bindings for primitives etc
        val initialFrameAddr = Kont0Addr
        SchemeState(Control.Ev(expr), initialEnv, initialStore, initialFrameAddr)

    /** Step from one state to another */
    def step(s0: State): Set[State] =
        import Control.*
        s0 match
            case SchemeState(Ev(expr), env, sto, kont)  => eval(expr, env, sto, kont)
            case SchemeState(Ap(value), env, sto, kont) => continue(value, env, sto, kont)

    /**
     * Push a frame on the conceptual stack of continuation frames
     *
     * @return
     *   a pair of the updated store (since continuations are store allocated), and the address at which the continuation has been allocated
     */
    def pushFrame(env: Env, sto: Sto, kont: Address, frame: Kont): (Sto, Address) = ???

    /** Evaluate the given expression */
    def eval(expr: Expr, env: Env, sto: Sto, kont: Address): Set[State] = expr match
        // (Ev(literal), env, sto, kont) ==> (Ap(inj(literal)), env, sto, kont)
        case lit: SchemeValue =>
          evalLiteralValue(lit, env, sto, kont)
        // (Ev((lambda (x) e)), env, sto, kont) ==> (Ap(inj(closure(x, e))), env, sto, kont)
        case lam: SchemeLambdaExp =>
          Set(SchemeState(Control.Ap(lattice.closure((lam, env.restrictTo(lam.fv)))), env, sto, kont))
        // (Ev(x), env, sto, kont) ==> (Ap(inj(v)), env, sto, kont)
        //    where: v = sto(env(x))
        case SchemeVar(id) =>
          evalVariable(id, env, sto, kont)
        // (Ev((set! x e)), env, sto, kont) ==> (Ev(e), env, sto, assgn(x) :: kont)
        case SchemeSet(id, exp, _) =>
          val (sto1, frame) = pushFrame(env, sto, kont, AssFrame(id))
          Set(SchemeState(Control.Ev(exp), env, sto1, frame))

        // (Ev((begin e1 e2 ... en)), env, sto, kont) ==> (Ev(e1), env, sto, beg(e2 ... en) :: kont)
        case SchemeBegin(exps, _) =>
          val (sto1, frame) = pushFrame(env, sto, kont, BegFrame(exps.tail))
          Set(SchemeState(Control.Ev(exps.head), env, sto1, frame))

        // (Ev((if prd csq alt)), env, sto, kont) ==> (Ev(prd), env, sto, ite(csq, alt) :: kont)
        case SchemeIf(prd, csq, alt, _) =>
          val (sto1, frame) = pushFrame(env, sto, kont, IteFrame(csq, alt))
          Set(SchemeState(Control.Ev(prd), env, sto1, frame))

        // (Ev((f x1 x2 ... xn), env, sto, kont) ==> (Ev(f), env, sto, fun(f, x1, ..., xn, bot, ()) :: kont)
        case fun @ SchemeFuncall(f, args, _) =>
          val (sto1, frame) = pushFrame(env, sto, kont, FunFrame(fun, args))
          Set(SchemeState(Control.Ev(f), env, sto1, frame))

    /**
     * Evaluate a literal value, these are evaluated to equivalent representations in the abstract domain. A string literal is allocated in the store
     * at a value address
     */
    private def evalLiteralValue(lit: SchemeValue, env: Env, sto: Sto, kont: Address): Set[State] =
        val (res, sto1) = lit.value match
            case sexp.Value.String(s) => ???
            // val (sptr, sto1) = allocateVal(exp, sto, lattice.string(s))
            // continue(kon, sptr, sto1)
            case sexp.Value.Integer(n)   => (lattice.number(n), sto)
            case sexp.Value.Real(r)      => (lattice.real(r), sto)
            case sexp.Value.Boolean(b)   => (lattice.bool(b), sto)
            case sexp.Value.Character(c) => (lattice.char(c), sto)
            case sexp.Value.Symbol(s)    => (lattice.symbol(s), sto)
            case sexp.Value.Nil          => (lattice.nil, sto)
            case lit                     => throw new Exception(s"Unsupported Scheme literal: $lit")

        Set(SchemeState(Control.Ap(res), env, sto1, kont))

    private def evalVariable(id: Identifier, env: Env, sto: Sto, kont: Address): Set[State] =
        val res: Value = env
          .lookup(id.name)
          .map(sto.lookup(_))
          .flatMap {
            case Some(Storable.V(v)) => Some(v)
            case _                   => None
          }
          .getOrElse(lattice.bottom)

        Set(SchemeState(Control.Ap(res), env, sto, kont))

    private def cond(
        value: Value,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: Address
      ): Set[State] =
        import Control.*
        val csqSt = if lattice.isTrue(value) then Set(SchemeState(Ev(csq), env, sto, kont)) else Set()
        val altSt = if lattice.isFalse(value) then Set(SchemeState(Ev(alt), env, sto, kont)) else Set()

        csqSt ++ altSt

    /** Apply the given continuation with the given value */
    def continue(value: Value, env: Env, sto: Sto, kont: Address): Set[State] = readSto(sto, kont) match
        case Storable.K(k) => // TODO: match against type of contuation frame
          k match
              // (Ap(v), env, sto, assgn(x) :: k) ==> (Ap(nil), env, sto', k)
              //    where sto' = sto [ env(x) -> v ]
              case AssFrame(id, Some(next)) =>
                val sto1 = writeSto(sto, env.lookup(id.name).get, Storable.V(value))
                Set(SchemeState(Control.Ap(lattice.nil), env, sto1, next))

              // (Ap(v), env, sto, beg(e1 e2 ... en) :: k) ==> (Ev(e1), env, sto, beg(e2 .. en) :: k)
              case BegFrame(e1 :: exps, Some(kont)) =>
                val (sto1, frame) = pushFrame(env, sto, kont, BegFrame(exps))
                Set(SchemeState(Control.Ev(e1), env, sto1, frame))

              // (Ap(v), env, sto, beg() :: k) ==> (Ap(v), env, sto, k)
              case BegFrame(List(), Some(kont)) =>
                Set(SchemeState(Control.Ap(value), env, sto, kont))

              // (Ap(true), env, sto, ite(csq, alt) :: k) ==> (Ev(csq), env, sto, k)
              // (Ap(false), env, sto, ite(csq, alt) :: k) ==> (Ev(alt), env, sto, k)
              case IteFrame(csq, alt, Some(kont)) =>
                cond(value, csq, alt, env, sto, kont)

              // (Ap(fv), env, sto, fun(f, a :: args) :: k) ==> (Ev(a), env, sto, FunArg(f, args, fv, List()) :: k)
              case FunFrame(f, arg :: args, Some(kont)) =>
                val (sto1, frame) = pushFrame(env, sto, kont, ArgFrame(f, args, value, List()))
                Set(SchemeState(Control.Ev(arg), env, sto, frame))

              // (Ap(fv), env, sto, fun(f, ()) :: k) ==> (Ev(a), env, sto, ret(env) :: k)
              case FunFrame(f, List(), Some(kont)) =>
                val (sto1, frame) = pushFrame(env, sto, kont, RetFrame(env))
                ??? // TODO: apply primitives/closures

              case ArgFrame(f, arg :: args, fv, argsV, Some(kont)) =>
                val (sto1, frame) = pushFrame(env, sto, kont, ArgFrame(f, args, fv, value :: argsV))
                Set(SchemeState(Control.Ev(arg), env, sto, frame))

              case ArgFrame(f, List(), fv, argsV, Some(kont)) =>
                ??? // TODO: apply primitives/closures and extend env and store with parameters

              // (Ap(v), env, sto, ret(env') :: k) ==> (Ap(v), env', sto, k)
              case RetFrame(env, Some(kont)) =>
                Set(SchemeState(Control.Ap(value), env, sto, kont))

              case _ => ???
        case _ => throw new Exception(s"address ${kont} is not a continuation")
