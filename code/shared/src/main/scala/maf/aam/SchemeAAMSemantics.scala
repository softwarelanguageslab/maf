package maf.aam

import maf.modular.ModAnalysis

import maf.util.*
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
abstract class SchemeAAMSemantics(prog: SchemeExp) extends AAMAnalysis with SchemeDomain:
    type Val = Value
    type Expr = SchemeExp
    type Kont = Frame
    type Sto = BasicStore[Address, Storable]
    type Lam = SchemeLambdaExp
    type State = SchemeState
    type Env = Environment[Address]
    type Ctx = Unit // TODO: fix

    override def analyzeWithTimeout(timeout: Timeout.T): Set[State] =
      analyze(prog, timeout)

    /** An instantation of the <code>SchemeInterpreterBridge</code> trait to support the builtin MAF Scheme primitives */
    private class InterpreterBridge(env: Env, private var sto: Sto, kont: Kont, t: Timestamp) extends SchemeInterpreterBridge[Val, Address]:
        def pointer(exp: SchemeExp): Address =
          alloc(exp.idn, env, sto, kont, t)

        def callcc(clo: Closure, pos: Position): Val = throw new Exception("not supported")
        def readSto(a: Address): Value =
            import Storable.*
            sto
              .lookup(a)
              .map {
                case V(v) => v
                case _    => lattice.bottom
              }
              .getOrElse(lattice.bottom)
        def writeSto(a: Address, value: Value): Unit =
          sto = sto.extend(a, Storable.V(value))
        def currentThread: TID =
          throw new Exception("unsupported")

        def updatedSto: Sto =
          sto

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
    given storableLattice: Lattice[Storable] with
        import Storable.*
        def bottom: Storable =
          Storable.V(lattice.bottom)

        def top: Storable = throw new Exception("storables have no single top element")
        def join(x: Storable, y: => Storable): Storable = (x, y) match
            case (V(v1), V(v2)) =>
              V(lattice.join(v1, v2))
            case (K(k1), K(k2)) =>
              K(k1 ++ k2)
            case _ =>
              throw new Exception(s"joining elements $x and $y not supported in storable lattice")

        def subsumes(x: Storable, y: => Storable): Boolean = throw new Exception("NYI")
        def eql[B: BoolLattice](x: Storable, y: Storable): B = throw new Exception("NYI")
        def show(v: Storable): String = v match
            case V(v1) => s"V($v1)"
            case K(k1) => s"K($k1)"

    /**
     * An address under which a continuation is stored. To preserve call-return semantics, this address should be (e, p) where e is the call targets
     * control and environment respectively (Gilray et al., 2016).
     */
    case class KontAddr(expr: SchemeExp, timestamp: Timestamp) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"KontAddr(${expr} ${timestamp})"

    case class RetAddr(expr: SchemeExp, timestamp: Timestamp) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"RetAddr(${expr} ${timestamp})"

    /** The location of the initial continuaton */
    case object Kont0Addr extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"Kont0Addr"

    /** An address on which values will be allocated */
    case class ValueAddr(lam: Lam, ctx: Ctx) extends Address:
        def idn: Identity = lam.idn
        def printable = true
        override def toString = s"ValueAddr(${lam}, ${ctx})"

    /** The address at which the values of function parameters are allocated */
    case class VarAddr(ide: Identity, ctx: Ctx) extends Address:
        def idn: Identity = ide
        def printable = true
        override def toString = s"VarAddr(${ide}, ${ctx})"

    /** An address on which location a primitive can be stored */
    case class PrimAddr(name: String) extends Address:
        def idn: Identity = Identity.none
        def printable = true
        override def toString = s"PrimAddr($name)"

    /** Read from the given address in the store, returns V(bottom) if no value is found at the given address. */
    def readSto(sto: Sto, addr: Address): Storable =
      sto.lookup(addr).getOrElse(Storable.V(lattice.bottom))

    /** Write to the given address in the store, returns the updated store */
    def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
      sto.extend(addr, value)

    private def preprocessProgram(program: List[SchemeExp]): SchemeExp =
        val originalProgram = program
        val preludedProgram = SchemePrelude.addPrelude(originalProgram)
        CSchemeUndefiner.undefine(preludedProgram)

    lazy val initialBds: Iterable[(String, Address, Storable)] =
      primitives.allPrimitives.view
        .map { case (name, p) =>
          (name, PrmAddr(name), Storable.V(lattice.primitive(p.name)))
        }

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
    case class LetFrame(
        // evaluated bindings
        evalBds: List[(Identifier, Value)],
        // the bindings that still need to be evaluated
        bindings: List[(Identifier, SchemeExp)],
        // the body of the let
        body: List[SchemeExp],
        next: Option[Address] = None)
        extends Frame:

        def link(next: Address): LetFrame =
          this.copy(next = Some(next))

    case class LetStarFrame(
        currentIdentifier: Identifier,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        next: Option[Address] = None)
        extends Frame:
        def link(next: Address): LetStarFrame =
          this.copy(next = Some(next))

    case class LetrecFrame(
        addresses: List[Address],
        valeus: List[Value],
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        next: Option[Address] = None)
        extends Frame:
        def link(next: Address): LetrecFrame =
          this.copy(next = Some(next))

    case object HltFrame extends Frame:
        def link(next: Address): Frame = this

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
    case class SchemeState(c: Control, e: Env, s: Sto, k: Kont, t: Timestamp)

    /** Inject the initial state for the given expression */
    def inject(expr: Expr): State =
        val initialEnv = BasicEnvironment(initialBds.map(p => (p._1, p._2)).toMap)
        val initialStore = BasicStore(initialBds.map(p => (p._2, p._3)).toMap).extend(Kont0Addr, Storable.K(Set(HltFrame)))
        SchemeState(Control.Ev(expr), initialEnv, initialStore, HltFrame, initialTime)

    /** Returns a string representation of the store. */
    def storeString(store: Sto, primitives: Boolean = false): String =
        val strings = store.content.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
        val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
        val size = filtered.size
        val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
        filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)

    /** Print a debug version of the given state */
    def printDebug(s: State, printStore: Boolean = false): Unit =
        println(s"control cmp ${s.c}, size of store ${s.s.asInstanceOf[BasicStore[Address, Val]].content.size}, kont addr ${s.k}")
        if printStore then println(storeString(s.s, false))

    /** Step from one state to another */
    def step(s0: State): Set[State] =
        import Control.*
        s0 match
            case SchemeState(Ev(expr), env, sto, kont, t)  => eval(expr, env, sto, kont, t)
            case SchemeState(Ap(value), env, sto, kont, t) => continue(value, env, sto, kont, t)

    /**
     * Push a frame on the conceptual stack of continuation frames
     *
     * @return
     *   a pair of the updated store (since continuations are store allocated), and the address at which the continuation has been allocated
     */
    protected def pushFrame(
        e: Expr,
        env: Env,
        sto: Sto,
        kont: Kont,
        frame: Kont,
        t: Timestamp
      ): (Sto, Kont, Timestamp) =
        val timestamp = tick(t, e, env, sto, kont)
        val addr = KontAddr(e, timestamp)
        val sto1 = writeSto(sto, addr, Storable.K(Set(kont)))
        (sto1, frame.link(addr), timestamp)

    protected def pushRetFrame(
        e: Expr,
        env: Env,
        sto: Sto,
        kont: Kont,
        frame: Kont,
        t: Timestamp
      ): (Sto, Kont, Timestamp) =
        val timestamp = tick(t, e, env, sto, kont)
        val addr = RetAddr(e, timestamp)
        val sto1 = writeSto(sto, addr, Storable.K(Set(kont)))
        (sto1, frame.link(addr), timestamp)

    /** Evaluate the given expression */
    def eval(expr: Expr, env: Env, sto: Sto, kont: Kont, t: Timestamp): Set[State] = expr match
        // (Ev(literal), env, sto, kont) ==> (Ap(inj(literal)), env, sto, kont)
        case lit: SchemeValue =>
          evalLiteralValue(lit, env, sto, kont, t)

        // (Ev((lambda (x) e)), env, sto, kont) ==> (Ap(inj(closure(x, e))), env, sto, kont)
        case lam: SchemeLambdaExp =>
          Set(SchemeState(Control.Ap(lattice.closure((lam, env.restrictTo(lam.fv)))), env, sto, kont, t))
        // (Ev(x), env, sto, kont) ==> (Ap(inj(v)), env, sto, kont)
        //    where: v = sto(env(x))
        case SchemeVar(id) =>
          evalVariable(id, env, sto, kont, t)
        // (Ev((set! x e)), env, sto, kont) ==> (Ev(e), env, sto, assgn(x) :: kont)
        case SchemeSet(id, exp, _) =>
          val (sto1, frame, t1) = pushFrame(exp, env, sto, kont, AssFrame(id), t)
          Set(SchemeState(Control.Ev(exp), env, sto1, frame, t1))

        // (Ev((begin e1 e2 ... en)), env, sto, kont) ==> (Ev(e1), env, sto, beg(e2 ... en) :: kont)
        case SchemeBegin(exps, _) =>
          val (sto1, frame, t1) = pushFrame(exps.head, env, sto, kont, BegFrame(exps.tail), t)
          Set(SchemeState(Control.Ev(exps.head), env, sto1, frame, t1))

        // (Ev((if prd csq alt)), env, sto, kont) ==> (Ev(prd), env, sto, ite(csq, alt) :: kont)
        case SchemeIf(prd, csq, alt, _) =>
          val (sto1, frame, t1) = pushFrame(prd, env, sto, kont, IteFrame(csq, alt), t)
          Set(SchemeState(Control.Ev(prd), env, sto1, frame, t1))

        // (Ev((f x1 x2 ... xn), env, sto, kont) ==> (Ev(f), env, sto, fun(f, x1, ..., xn, bot, ()) :: kont)
        case fun @ SchemeFuncall(f, args, _) =>
          val (sto1, frame, t1) = pushFrame(f, env, sto, kont, FunFrame(fun, args), t)
          Set(SchemeState(Control.Ev(f), env, sto1, frame, t1))

        case SchemeLet(bindings, body, _) =>
          evaluateLet(List(), env, sto, kont, bindings, body, t)

        case SchemeLetStar(bindings, body, _) =>
          evaluateLetStar(env, sto, kont, bindings, body, t)

        case SchemeLetrec(bindings, body, _) =>
          val env1 = bindings.map(_._1).foldLeft(env)((newEnv, identifier) => newEnv.extend(identifier.name, alloc(identifier.idn, env, sto, kont, t)))
          evaluateLetrec(Nil, Nil, env1, sto, kont, bindings, body, t)

        // unsupported
        case _: SchemeAssert =>
          Set()

        case _ => throw new Exception("unsupported exception")

    /**
     * Evaluate a literal value, these are evaluated to equivalent representations in the abstract domain. A string literal is allocated in the store
     * at a value address
     */
    private def evalLiteralValue(lit: SchemeValue, env: Env, sto: Sto, kont: Kont, t: Timestamp): Set[State] =
        val (res, sto1) = lit.value match
            case sexp.Value.String(s) =>
              val address = alloc(lit.idn, env, sto, kont, t)
              val sto1 = writeSto(sto, address, Storable.V(lattice.string(s)))
              (lattice.pointer(address), sto1)

            case sexp.Value.Integer(n)   => (lattice.number(n), sto)
            case sexp.Value.Real(r)      => (lattice.real(r), sto)
            case sexp.Value.Boolean(b)   => (lattice.bool(b), sto)
            case sexp.Value.Character(c) => (lattice.char(c), sto)
            case sexp.Value.Symbol(s)    => (lattice.symbol(s), sto)
            case sexp.Value.Nil          => (lattice.nil, sto)
            case lit                     => throw new Exception(s"Unsupported Scheme literal: $lit")

        Set(SchemeState(Control.Ap(res), env, sto1, kont, t))

    private def evalVariable(id: Identifier, env: Env, sto: Sto, kont: Kont, t: Timestamp): Set[State] =
        val res: Value = env
          .lookup(id.name)
          .map(sto.lookup(_))
          .flatMap {
            case Some(Storable.V(v)) => Some(v)
            case _                   => None
          }
          .getOrElse(lattice.bottom)

        Set(SchemeState(Control.Ap(res), env, sto, kont, t))

    /** Evaluate a non-empty sequence of expression */
    private def evaluate_sequence(env: Env, sto: Sto, kont: Kont, sequence: List[SchemeExp], t: Timestamp): Set[State] =
        assert(!sequence.isEmpty)
        val (sto1, frame, t1) = pushFrame(sequence.head, env, sto, kont, BegFrame(sequence.tail), t)
        Set(SchemeState(Control.Ev(sequence.head), env, sto1, frame, t1))

    private def evaluateLet(
        evlBds: List[(Identifier, Value)],
        env: Env,
        sto: Sto,
        kont: Kont,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        t: Timestamp
      ): Set[State] =
      bindings match
          case List() =>
            val addresses =
              bindings.map(_._1).map((current) => alloc(current.idn, env, sto, kont, t))
            val env1: Env =
              evlBds
                .map(_._1)
                .zip(addresses)
                .foldLeft(env)((envNew, current) => envNew.extend(current._1.name, current._2))
            val sto1: Sto =
              evlBds
                .map(_._2)
                .zip(addresses)
                .foldLeft(sto)((sto, current) => writeSto(sto, current._2, Storable.V(current._1)))

            val (sto2, frame, t1) = pushRetFrame(body.head, env1, sto1, kont, RetFrame(env), t)
            evaluate_sequence(env, sto2, frame, body, t1)

          case binding :: bindings =>
            val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetFrame(evlBds, binding :: bindings, body), t)
            Set(SchemeState(Control.Ev(binding._2), env, sto1, frame, t1))

    private def evaluateLetStar(
        env: Env,
        sto: Sto,
        kont: Kont,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        t: Timestamp
      ): Set[State] =
      bindings match
          case List() =>
            // evaluate the body, the environment is already correctly extended by bindings in the previous evaluation steps
            val (sto1, frame, t1) = pushRetFrame(body.head, env, sto, kont, RetFrame(env), t)
            evaluate_sequence(env, sto1, frame, body, t1)
          case binding :: bindings =>
            val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetStarFrame(binding._1, bindings, body), t)
            Set(SchemeState(Control.Ev(binding._2), env, sto1, frame, t1))

    private def evaluateLetrec(
        addresses: List[Address],
        values: List[Value],
        env: Env,
        sto: Sto,
        kont: Kont,
        bindings: List[(Identifier, SchemeExp)],
        body: List[SchemeExp],
        t: Timestamp,
      ): Set[State] =
      bindings match
          case List() =>
            // the enviroment already contains the necessary bindings
            val sto1 = addresses.zip(values).foldLeft(sto)((sto, current) => writeSto(sto, current._1, Storable.V(current._2)))
            evaluate_sequence(env, sto1, kont, body, t)

          case binding :: bindings =>
            val addresses1 = alloc(binding._1.idn, env, sto, kont, t) :: addresses
            val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetrecFrame(addresses1, values, bindings, body), t)
            Set(SchemeState(Control.Ev(binding._2), env, sto1, frame, t1))

    private def applyFun(
        fexp: SchemeFuncall,
        func: Value,
        argv: List[Value],
        env: Env,
        sto: Sto,
        kon: Kont,
        t: Timestamp
      ): Set[State] =
        val closures = applyClo(fexp, func, argv, env, sto, kon, t)
        val functions = applyPrim(fexp, func, argv, env, sto, kon, t)
        closures ++ functions

    private def applyClo(
        fexp: SchemeFuncall,
        func: Value,
        argv: List[Value],
        env: Env,
        sto: Sto,
        kon: Kont,
        t: Timestamp
      ): Set[State] =
      // TODO: introduce contexts to support things like k-cfa
      lattice.getClosures(func).flatMap {
        case (lam, lex: Env @unchecked) if lam.check(argv.size) =>
          // push a frame to restore the environment upon return
          val (sto0, frame, t0) = pushRetFrame(fexp, env, sto, kon, RetFrame(env), t)

          // split in fixed an variable number of arguments
          val (fx, vra) = argv.zip(fexp.args).splitAt(lam.args.length)
          val ctx = () // TODO
          // add the fixed arguments on addresses in the store
          val sto1 = lam.args.zip(fx).foldLeft(sto0) { case (sto, (par, (value, _))) =>
            writeSto(sto, VarAddr(par.idn, ctx), Storable.V(value))
          }
          // add variable arguments as a list to a particular address in the store
          val sto2 = lam.varArgId match
              case Some(id) =>
                val (vlu, newsto) = allocList(vra, env, sto1, kon, t0)
                writeSto(newsto, VarAddr(id.idn, ctx), Storable.V(vlu))
              case _ => sto1

          // extend the environment with the correct bindigs
          val pars = (lam.args ++ lam.varArgId.map(List(_)).getOrElse(List()))
          val env1 = pars.foldLeft(env)((env, par) => env.extend(par.name, VarAddr(par.idn, ctx)))
          // and evaluate the body
          evaluate_sequence(env1, sto2, frame, lam.body, t0)
        case (lam, lex) =>
          println(s"Applied with invalid number of arguments ${argv.size}")
          Set()
      }

    /** Apply the given value as a primitive function (if the value is a primitive function) */
    private def applyPrim(
        fexp: SchemeExp,
        func: Value,
        argv: List[Value],
        env: Env,
        sto: Sto,
        kon: Kont,
        t: Timestamp,
      ): Set[State] =
      lattice.getPrimitives(func).flatMap { name =>
          val primitive = primitives(name)
          given bridge: InterpreterBridge = InterpreterBridge(env, sto, kon, t)
          primitive.callMF(fexp, argv) match
              // the primitive is successfull apply the continuation with the value returned from the primitive
              case MayFailSuccess(vlu) =>
                val sto1 = bridge.updatedSto
                Set(SchemeState(Control.Ap(vlu), env, sto1, kon, t))
              case MayFailBoth(vlu, _) =>
                val sto1 = bridge.updatedSto
                Set(SchemeState(Control.Ap(vlu), env, sto1, kon, t))
              // executing the primitive is unsuccessfull, no successors states are generated
              case MayFailError(_) => Set()
      }

    private def cond(
        value: Value,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: Kont,
        t: Timestamp
      ): Set[State] =
        import Control.*
        val csqSt = if lattice.isTrue(value) then Set(SchemeState(Ev(csq), env, sto, kont, t)) else Set()
        val altSt = if lattice.isFalse(value) then Set(SchemeState(Ev(alt), env, sto, kont, t)) else Set()
        csqSt ++ altSt

    private def allocList(items: List[(Value, SchemeExp)], env: Env, sto: Sto, kont: Kont, t: Timestamp): (Value, Sto) = items match
        case Nil => (lattice.nil, sto)
        case (vlu, exp) :: rest =>
          val (tail, sto1) = allocList(rest, env, sto, kont, t)
          allocCons(exp, vlu, tail, env, sto1, kont, t)

    private def allocCons(
        exp: SchemeExp,
        car: Value,
        cdr: Value,
        env: Env,
        sto: Sto,
        kont: Kont,
        t: Timestamp
      ): (Value, Sto) =
        val addr = alloc(exp.idn, env, sto, kont, t) // TODO: check whether a seperate addr is needed for cons
        val sto1 = writeSto(sto, addr, Storable.V(lattice.cons(car, cdr)))
        (lattice.pointer(addr), sto1)

    protected def continueWith(sto: Sto, kont: Address)(f: (Kont) => SchemeState): Set[State] =
      readSto(sto, kont) match {
        case Storable.K(ks) => ks.map(f)
        case _              => Set()
      }

    protected def continueWiths(sto: Sto, kont: Address)(f: (Kont) => Set[SchemeState]): Set[State] =
      readSto(sto, kont) match {
        case Storable.K(ks) => ks.flatMap(f)
        case _              => Set()
      }

    /** Apply the given continuation with the given value */
    def continue(value: Value, env: Env, sto: Sto, kont: Kont, t: Timestamp): Set[State] = kont match
        // (Ap(v), env, sto, assgn(x) :: k) ==> (Ap(nil), env, sto', k)
        //    where sto' = sto [ env(x) -> v ]
        case AssFrame(id, Some(next)) =>
          val sto1 = writeSto(sto, env.lookup(id.name).get, Storable.V(value))
          continueWith(sto, next)(SchemeState(Control.Ap(lattice.nil), env, sto1, _, t))

        // (Ap(v), env, sto, beg(e1 e2 ... en) :: k) ==> (Ev(e1), env, sto, beg(e2 .. en) :: k)
        case BegFrame(e1 :: exps, Some(addr)) =>
          continueWith(sto, addr) { kont =>
              val (sto1, frame, t1) = pushFrame(e1, env, sto, kont, BegFrame(exps), t)
              SchemeState(Control.Ev(e1), env, sto1, frame, t1)
          }

        // (Ap(v), env, sto, beg() :: k) ==> (Ap(v), env, sto, k)
        case BegFrame(List(), Some(addr)) =>
          continueWith(sto, addr) { kont =>
            SchemeState(Control.Ap(value), env, sto, kont, t)
          }

        // (Ap(true), env, sto, ite(csq, alt) :: k) ==> (Ev(csq), env, sto, k)
        // (Ap(false), env, sto, ite(csq, alt) :: k) ==> (Ev(alt), env, sto, k)
        case IteFrame(csq, alt, Some(addr)) =>
          continueWiths(sto, addr)(cond(value, csq, alt, env, sto, _, t))

        // (Ap(fv), env, sto, fun(f, a :: args) :: k) ==> (Ev(a), env, sto, FunArg(f, args, fv, List()) :: k)
        case FunFrame(f, arg :: args, Some(addr)) =>
          continueWith(sto, addr) { kont =>
              val (sto1, frame, t1) = pushFrame(arg, env, sto, kont, ArgFrame(f, args, value, List()), t)
              SchemeState(Control.Ev(arg), env, sto1, frame, t1)
          }

        // (Ap(fv), env, sto, fun(f, ()) :: k) ==> (Ev(a), env, sto, ret(env) :: k)
        case FunFrame(f, List(), Some(addr)) =>
          continueWiths(sto, addr) { kont =>
            applyFun(f, value, List(), env, sto, kont, t)
          }

        case ArgFrame(f, arg :: args, fv, argsV, Some(addr)) =>
          continueWith(sto, addr) { kont =>
              val (sto1, frame, t1) = pushFrame(arg, env, sto, kont, ArgFrame(f, args, fv, value :: argsV), t)
              SchemeState(Control.Ev(arg), env, sto1, frame, t1)
          }

        case ArgFrame(f, List(), fv, argsV, Some(addr)) =>
          continueWiths(sto, addr) { kont =>
            applyFun(f, fv, (value :: argsV).reverse, env, sto, kont, t)
          }

        // (Ap(v), env, sto, ret(env') :: k) ==> (Ap(v), env', sto, k)
        case RetFrame(env, Some(addr)) =>
          continueWith(sto, addr) { kont =>
            SchemeState(Control.Ap(value), env, sto, kont, t)
          }

        case LetFrame(evalBds, _ :: bindings, body, Some(addr)) =>
          continueWiths(sto, addr) { kont =>
            evaluateLet(evalBds, env, sto, kont, bindings, body, t)
          }

        case LetStarFrame(currentIdentifier, restBindings, body, Some(addr)) =>
          continueWiths(sto, addr) { kont =>
              val addr = alloc(currentIdentifier.idn, env, sto, kont, t)
              val env1 = env.extend(currentIdentifier.name, addr)
              val sto1 = writeSto(sto, addr, Storable.V(value))
              evaluateLetStar(env1, sto1, kont, restBindings, body, t)
          }

        case LetrecFrame(addresses, values, bindings, body, Some(addr)) =>
          continueWiths(sto, addr) { kont =>
            evaluateLetrec(addresses, value :: values, env, sto, kont, bindings, body, t)
          }

        case HltFrame => Set()
