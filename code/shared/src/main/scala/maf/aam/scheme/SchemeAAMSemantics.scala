package maf.aam.scheme

import maf.aam.{AAMAnalysis, AnalysisResult, GraphElementAAM}

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
import maf.util.graph.{Graph, GraphElement}
import maf.util.graph.Colors

/** An AAM style semantics for Scheme */
trait BaseSchemeAAMSemantics(prog: SchemeExp) extends AAMAnalysis, SchemeDomain { outer =>
  type Val
  type LatVal = Value
  type Expr = SchemeExp
  type Kont = Frame
  // type Sto = BasicStore[Address, Storable]
  type Lam = SchemeLambdaExp
  type State = SchemeState
  type Env = Environment[Address]
  type Ctx = Unit // TODO: fix
  type Error = SchemeError
  type Ext

  lazy val initialEnv = BasicEnvironment(initialBds.map(p => (p._1, p._2)).toMap)
  lazy val initialStore: Sto

  override def analyzeWithTimeout[G](timeout: Timeout.T, graph: G)(using Graph[G, GraphElementAAM, GraphElement]): AnalysisResult[G, Val, Conf] =
    analyze(prog, graph, timeout)

  /** An instantation of the <code>SchemeInterpreterBridge</code> trait to support the builtin MAF Scheme primitives */
  private class InterpreterBridge(env: Env, private var sto: Sto, kont: KonA, t: Timestamp, ext: Ext) extends SchemeInterpreterBridge[LatVal, Address]:
      def pointer(exp: SchemeExp): Address =
        alloc(exp.idn, env, sto, kont, t)

      def callcc(clo: Closure, pos: Position): LatVal = throw new Exception("not supported")
      def readSto(a: Address): LatVal =
        project(readStoV(sto, a, ext))

      def writeSto(a: Address, value: LatVal): Unit =
        sto = outer.writeSto(sto, a, Storable.V(value))

      def currentThread: TID =
        throw new Exception("unsupported")

      def updatedSto: Sto =
        sto

  /**
   * The `Storable` enum represents values that can be stored in the store. To enable approximating semantics we define a lattice over these storable
   * values below.
   */
  enum Storable:
      /** We can store values in the store */
      case V(v: LatVal)

      /** We can also store continuations in the store */
      case K(k: Set[Kont])

  trait SchemeError
  case class InvalidNumberOfArguments(fexp: SchemeFuncall, got: Int, expected: Int) extends SchemeError
  case class AssertionFailed(location: Identity) extends SchemeError

  protected def error(err: SchemeError, sto: Sto, kon: KonA, t: Timestamp, ext: Ext): Set[State] =
    Set(SchemeState(Control.HltE(err), sto, kon, t, ext))

  /** Inject the values from the lattice's abstract domain in the (possibly extended) domain of a sub analysis */
  def inject(v: LatVal): Val

  /** Project the (possibly extended) domain of a sub analysis to the lattice domain */
  def project(v: Val): LatVal

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
  case class KontAddr(expr: SchemeExp, env: Env, timestamp: Timestamp) extends Address:
      def idn: Identity = expr.idn
      def printable = true
      override def toString = s"KontAddr(${expr} ${timestamp})"

  case class FunRetAddr(expr: SchemeExp, env: Env, timestamp: Timestamp) extends Address:
      def idn: Identity = expr.idn
      def printable = true
      override def toString = s"FunRetAddr(${expr} ${timestamp})"

  /** The location of the initial continuaton */
  case object Kont0Addr extends Address:
      def idn: Identity = Identity.none
      def printable = true
      override def toString = s"Kont0Addr"

  /** An address on which values will be allocated */
  case class ValAddr(lam: Lam, ctx: Ctx) extends Address:
      def idn: Identity = lam.idn
      def printable = true
      override def toString = s"ValAddr(${lam}, ${ctx})"

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
  def readSto(sto: Sto, addr: Address): Storable

  def readStoV(sto: Sto, addr: Address, ext: Ext): Val =
    readSto(sto, addr) match
        case Storable.V(v) => inject(v)
        case _             => inject(lattice.bottom)

  /** Write to the given address in the store, returns the updated store */
  def writeSto(sto: Sto, addr: Address, value: Storable): Sto

  /** Write a value to the given address in the store, returns the updated store */
  def writeStoV(sto: Sto, addr: Address, value: Val, ext: Ext): (Sto, Ext) =
    (writeSto(sto, addr, Storable.V(project(value))), ext)

  private def preprocessProgram(program: List[SchemeExp]): SchemeExp =
      val originalProgram = program
      val preludedProgram = SchemePrelude.addPrelude(originalProgram)
      CSchemeUndefiner.undefine(preludedProgram)

  lazy val initialBds: Iterable[(String, Address, Storable)] =
    primitives.allPrimitives.view
      .map { case (name, p) =>
        (name, PrmAddr(name), Storable.V(lattice.primitive(p.name)))
      }

  protected trait Frame:
      def link(next: Address | Frame): Frame
      def name: String = "unknown frame"

  case class EmptyFrame(next: Option[Address | Frame] = None) extends Frame:
      def link(next: Address | Frame): EmptyFrame = this.copy(next = Some(next))
      override def name: String = "EmptyFrame"

  case class AssFrame(id: Identifier, env: Env, next: Option[Address | Frame] = None) extends Frame:
      def link(next: Address | Frame): AssFrame =
        AssFrame(id, env, Some(next))
      override def name: String = "AssFrame"

  case class BegFrame(exps: List[Expr], env: Env, cross: Boolean, next: Option[Address | Frame] = None) extends Frame:
      def link(next: Address | Frame): BegFrame =
        this.copy(next = Some(next))
      override def name: String = "BegFrame"
  case class IteFrame(csq: Expr, alt: Expr, env: Env, next: Option[Address | Frame] = None) extends Frame:
      def link(next: Address | Frame): IteFrame =
        this.copy(next = Some(next))
      override def name: String = "IteFrame"
  case class LetFrame(
      // evaluated bindings
      evalBds: List[(Identifier, Val)],
      // the bindings that still need to be evaluated
      bindings: List[(Identifier, SchemeExp)],
      // the body of the let
      body: List[SchemeExp],
      env: Env,
      next: Option[Address | Frame] = None)
      extends Frame:

      def link(next: Address | Frame): LetFrame =
        this.copy(next = Some(next))

      override def name: String = "LetFrame"

  case class LetStarFrame(
      currentIdentifier: Identifier,
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      env: Env,
      next: Option[Address | Frame] = None)
      extends Frame:
      def link(next: Address | Frame): LetStarFrame =
        this.copy(next = Some(next))

      override def name: String = "LetStarFrame"

  case class LetrecFrame(
      currentAddress: Address,
      addresses: List[Address],
      values: List[Val],
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      env: Env,
      next: Option[Address | Frame] = None)
      extends Frame:
      def link(next: Address | Frame): LetrecFrame =
        this.copy(next = Some(next))

      override def name: String = "LetRecFrame"

  case class AssertFrame(
      idn: Identity,
      env: Env,
      next: Option[Address | Frame] = None)
      extends Frame:
      def link(next: Address | Frame): AssertFrame =
        this.copy(next = Some(next))

      override def name: String = "AssertFrame"

  case object HltFrame extends Frame:
      def link(next: Address | Frame): Frame = this
      override def name: String = "Hlt"

  /**
   * A continuation for evaluating a function.
   *
   * @param f
   *   the original Scheme expression corresponding to the function call
   * @param args
   *   the list of arguments that still need to be evaluated
   */
  case class FunFrame(f: SchemeFuncall, args: List[SchemeExp], env: Env, next: Option[Address | Frame] = None) extends Frame:
      def link(next: Address | Frame): FunFrame =
        this.copy(next = Some(next))

      override def name: String = "FunFrame"

  case class ArgFrame(
      f: SchemeFuncall,
      args: List[SchemeExp],
      fv: Val,
      argV: List[Val],
      env: Env,
      next: Option[Address | Frame] = None)
      extends Frame:
      def link(next: Address | Frame): ArgFrame =
        this.copy(next = Some(next))

      override def name: String = "ArgFrame"

  /** The control of the CESK* machine */
  enum Control:
      /** Instruction to evaluate the given expression */
      case Ev(expr: Expr, env: Env)

      /** Instruction to apply the continuation that is on top of the continuation stack */
      case Ap(value: Val)

      /** A control component at the boundary of the function call */
      case Fn(c: Control)

      /** An error halting state */
      case HltE(error: Error)

      /** A return value of a function, where the return value is allocated in the store */
      case Ret(value: Address)

  /** Provide a default "empty" extension to the state of the AAM-based analysis */
  protected def emptyExt: Ext

  /** A state of the Scheme AAM machine */
  case class SchemeState(c: Control, s: Sto, k: Address | Frame, t: Timestamp, extra: Ext):
      override def toString: String =
          val control = c match {
            case Control.Ev(expr, env) => s"ev(${expr.toString})"
            case Control.Ap(vlu)       => s"ap(${vlu.toString})"
            case Control.Fn(con)       => s"fn(${con})"
            case Control.HltE(error)   => s"err(${error})"
            case Control.Ret(addr) =>
              val vlu = readStoV(s, addr, extra)
              s"addr($vlu)"
          }

          s"($control, $k)"

      /** Map the given function to the value, only if it is an Ap control */
      def mapValue(f: Val => Val): SchemeState =
        c match
            case Control.Ap(vlu) => this.copy(c = Control.Ap(f(vlu)))
            case _               => this

  /** Returns a string representation of the store. */
  def storeString(store: Sto, primitives: Boolean = false): String =
    "cannot print store in base analysis"

  /** Print a debug version of the given state */
  def printDebug(s: Conf, printStore: Boolean = false): Unit =
    println("printDebug not supported in base analysis")

  def compareStates(s1: State, s2: State): Boolean =
      println("comparison not supported in base analysis")
      true

  def isFinal(st: State): Boolean =
    (st.k == Kont0Addr && (st.c match { case Control.Ap(_) => true; case Control.Ret(_) => true; case _ => false }))

  def extractValue(st: State): Option[Val] =
    st.c match
        case Control.Ap(v) => Some(v)
        case Control.Ret(addr) =>
          val vlu = readStoV(st.s, addr, st.extra)
          if lattice.isBottom(project(vlu)) then None else Some(vlu)
        case _ => None

  protected def asGraphElement(c: Control, k: KonA, s: Sto, ext: Ext, hsh: Int): GraphElementAAM = c match
      case Control.Ev(e, _) => GraphElementAAM(hsh, label = s"ev($e)", color = Colors.Yellow, data = "")
      case Control.Ap(v) =>
        val kontName = readKonts(s, k).map(_.name).mkString(",")
        GraphElementAAM(s.hashCode, label = s"ap($v, $kontName)", color = Colors.Red, data = "")
      case Control.Ret(addr) =>
        val vlu = readStoV(s, addr, ext)
        GraphElementAAM(s.hashCode, label = s"ret($vlu)", color = Colors.Red, data = "")
      case Control.Fn(c)     => asGraphElement(c, k, s, ext, hsh)
      case Control.HltE(err) => GraphElementAAM(s.hashCode, label = s"err($err)", color = Colors.Pink, data = "")

  def stepDirect(ss: Set[State]): Set[State] = ss

  /** Step from one state to another */
  def step(s0: State): Set[State] =
      import Control.*
      val successors = s0 match
          case SchemeState(Ev(expr, env), sto, kont, t, ext) => eval(expr, env, sto, kont, t, ext)
          case SchemeState(Ap(value), sto, kont, t, ext)     => continue(value, sto, kont, t, ext)
          case SchemeState(Ret(addr), sto, kont, t, ext) =>
            continue(readStoV(sto, addr, ext), sto, kont, t, ext)
          case s @ SchemeState(HltE(error), _, _, _, _) =>
            //println(s"ERR: ${error}")
            registerError(error, s)
            Set() // HltE is a final state

          case s @ SchemeState(Fn(contr), sto, kont, t, ext) => step(s.copy(c = contr))

      // try to atomically step the successor
      stepDirect(successors)

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
      next: KonA,
      frame: Kont,
      t: Timestamp
    ): (Sto, KonA, Timestamp) =
      val timestamp = tick(t, e, sto, next)
      val addr = KontAddr(e, env, timestamp)
      val sto1 = writeSto(sto, addr, Storable.K(Set(frame.link(next))))
      (sto1, addr, timestamp)

  /**
   * Push a return frame on the conceptual stack of continuation frames
   *
   * @return
   *   a pair of the updated store (since continuations are store allocated), and the address at which the continuation has been allocated
   */
  protected def pushFrameRet(
      e: Expr,
      env: Env,
      sto: Sto,
      next: KonA,
      frame: Kont,
      t: Timestamp
    ): (Sto, Address, Timestamp) =
      val timestamp = tick(t, e, sto, next)
      val addr = FunRetAddr(e, env, timestamp)
      val sto1 = writeSto(sto, addr, Storable.K(Set(frame.link(next))))
      (sto1, addr, timestamp)

  /** Evaluate the given expression */
  def eval(
      expr: Expr,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] = expr match
      // (Ev(literal), env, sto, kont) ==> (Ap(inj(literal)), env, sto, kont)
      case lit: SchemeValue =>
        evalLiteralVal(lit, env, sto, kont, t, ext)

      // (Ev((lambda (x) e)), env, sto, kont) ==> (Ap(inj(closure(x, e))), env, sto, kont)
      case lam: SchemeLambdaExp =>
        ap(inject(lattice.closure((lam, env.restrictTo(lam.fv)))), sto, kont, t, ext)

      // (Ev(x), env, sto, kont) ==> (Ap(inj(v)), env, sto, kont)
      //    where: v = sto(env(x))
      case SchemeVar(id) =>
        evalVariable(id, env, sto, kont, t, ext)
      // (Ev((set! x e)), env, sto, kont) ==> (Ev(e), env, sto, assgn(x) :: kont)
      case SchemeSet(id, exp, _) =>
        val (sto1, frame, t1) = pushFrame(exp, env, sto, kont, AssFrame(id, env), t)
        ev(exp, env, sto1, frame, t1, ext)

      // (Ev((begin e1 e2 ... en)), env, sto, kont) ==> (Ev(e1), env, sto, beg(e2 ... en) :: kont)
      case SchemeBegin(exps, _) =>
        evaluate_sequence(env, sto, kont, exps, t, ext)

      // (Ev((if prd csq alt)), env, sto, kont) ==> (Ev(prd), env, sto, ite(csq, alt) :: kont)
      case SchemeIf(prd, csq, alt, _) =>
        val (sto1, frame, t1) = pushFrame(prd, env, sto, kont, IteFrame(csq, alt, env), t)
        ev(prd, env, sto1, frame, t1, ext)

      // (Ev((f x1 x2 ... xn), env, sto, kont) ==> (Ev(f), env, sto, fun(f, x1, ..., xn, bot, ()) :: kont)
      case fun @ SchemeFuncall(f, args, _) =>
        val (sto1, frame, t1) = pushFrame(f, env, sto, kont, FunFrame(fun, args, env), t)
        ev(f, env, sto1, frame, t1, ext)

      case SchemeLet(bindings, body, _) =>
        evaluateLet(List(), env, sto, kont, bindings, body, t, ext)

      case SchemeLetStar(bindings, body, _) =>
        evaluateLetStar(env, sto, kont, bindings, body, t, ext)

      case SchemeLetrec(bindings, body, _) =>
        val env1 = bindings.map(_._1).foldLeft(env)((newEnv, identifier) => newEnv.extend(identifier.name, alloc(identifier.idn, env, sto, kont, t)))
        evaluateLetrec(Nil, Nil, env1, sto, kont, bindings, body, t, ext)

      case SchemeAssert(exp, idn) =>
        evalAssert(exp, idn, env, sto, kont, t, ext)

      case _ => throw new Exception("unsupported exception")

  /** Evaluate an (assert exp) expression */
  protected def evalAssert(
      exp: SchemeExp,
      idn: Identity,
      env: Env,
      sto: Sto,
      kon: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
      val (sto1, frame, t1) = pushFrame(exp, env, sto, kon, AssertFrame(idn, env), t)
      ev(exp, env, sto1, frame, t1, ext)

  /**
   * Evaluate a literal value, these are evaluated to equivalent representations in the abstract domain. A string literal is allocated in the store at
   * a value address
   */
  protected def evalLiteralVal(
      lit: SchemeValue,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
      val (res, sto1, ext1) = lit.value match
          case sexp.Value.String(s) =>
            val address = alloc(lit.idn, env, sto, kont, t)
            val (sto1, ext1) = writeStoV(sto, address, inject(lattice.string(s)), ext)
            (lattice.pointer(address), sto1, ext1)

          case sexp.Value.Integer(n)   => (lattice.number(n), sto, ext)
          case sexp.Value.Real(r)      => (lattice.real(r), sto, ext)
          case sexp.Value.Boolean(b)   => (lattice.bool(b), sto, ext)
          case sexp.Value.Character(c) => (lattice.char(c), sto, ext)
          case sexp.Value.Symbol(s)    => (lattice.symbol(s), sto, ext)
          case sexp.Value.Nil          => (lattice.nil, sto, ext)
          case lit                     => throw new Exception(s"Unsupported Scheme literal: $lit")

      ap(inject(res), sto1, kont, t, ext1)

  private def evalVariable(
      id: Identifier,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
      val res: Val = env
        .lookup(id.name)
        .map(readStoV(sto, _, ext))
        .getOrElse {
          println(s"ERR: undefined variable $id")
          inject(lattice.bottom)
        }

      ap(res, sto, kont, t, ext)

  /**
   * Evaluate a non-empty sequence of expression
   *
   * @param cross
   *   set to true if the evaluation of the sequence crosses function boundaries (e.g. if this is the evaluation of the body of a function)
   */
  protected def evaluate_sequence(
      env: Env,
      sto: Sto,
      kont: KonA,
      sequence: List[SchemeExp],
      t: Timestamp,
      ext: Ext,
      cross: Boolean = false
    ): Set[State] =
      val (sto1, frame, t1) = sequence match
          case List(x) => (sto, kont, t)
          case x :: xs => pushFrame(sequence.head, env, sto, kont, BegFrame(sequence.tail, env, false), t)
          case Nil     => throw new Exception("malformed program: sequence cannot be empty")

      ev(sequence.head, env, sto1, frame, t1, ext, cross)

  private def evaluateLet(
      evlBds: List[(Identifier, Val)],
      env: Env,
      sto: Sto,
      kont: KonA,
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    bindings match
        case List() =>
          val addresses =
            evlBds.map(_._1).map((current) => alloc(current.idn, env, sto, kont, t))
          val env1: Env =
            evlBds
              .map(_._1)
              .zip(addresses)
              .foldLeft(env)((envNew, current) => envNew.extend(current._1.name, current._2))
          val (sto1, ext1): (Sto, Ext) =
            evlBds
              .map(_._2)
              .zip(addresses)
              .foldLeft((sto, ext)) { case ((sto, ext), current) =>
                writeStoV(sto, current._2, current._1, ext)
              }

          evaluate_sequence(env1, sto1, kont, body, t, ext1)

        case binding :: bindings =>
          val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetFrame(evlBds, binding :: bindings, body, env), t)
          ev(binding._2, env, sto1, frame, t1, ext)

  private def evaluateLetStar(
      env: Env,
      sto: Sto,
      kont: KonA,
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    bindings match
        case List() =>
          // evaluate the body, the environment is already correctly extended by bindings in the previous evaluation steps
          evaluate_sequence(env, sto, kont, body, t, ext)
        case binding :: bindings =>
          // the environment is already extended (or should be) in the "continue" function
          val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetStarFrame(binding._1, bindings, body, env), t)
          ev(binding._2, env, sto1, frame, t1, ext)

  private def evaluateLetrec(
      addresses: List[Address],
      values: List[Val],
      env: Env,
      sto: Sto,
      kont: KonA,
      bindings: List[(Identifier, SchemeExp)],
      body: List[SchemeExp],
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    bindings match
        case List() =>
          // the enviroment already contains the necessary bindings
          // the store also already contains the necessary bindings
          // val sto1 = addresses.zip(values).foldLeft(sto)((sto, current) => writeStoV(sto, current._1, current._2))
          evaluate_sequence(env, sto, kont, body, t, ext)

        case binding :: bindings =>
          val address = alloc(binding._1.idn, env, sto, kont, t)
          val addresses1 = address :: addresses
          val (sto1, frame, t1) = pushFrame(binding._2, env, sto, kont, LetrecFrame(address, addresses1, values, bindings, body, env), t)
          ev(binding._2, env, sto1, frame, t1, ext)

  protected def applyFun(
      fexp: SchemeFuncall,
      func: Val,
      argv: List[Val],
      env: Env,
      sto: Sto,
      kon: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
      val closures = applyClo(fexp, func, argv, env, sto, kon, t, ext)
      val functions = applyPrim(fexp, func, argv, env, sto, kon, t, ext)
      closures ++ functions

  protected def invalidArity(
      fexp: SchemeFuncall,
      got: Int,
      expected: Int,
      sto: Sto,
      kon: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    Set(
      SchemeState(Control.HltE(
                    InvalidNumberOfArguments(fexp, got, expected)
                  ),
                  sto,
                  kon,
                  t,
                  ext
      )
    )

  protected def bindArgs(
      fexp: SchemeFuncall,
      argv: List[Val],
      lam: SchemeLambdaExp,
      lex: Env,
      sto0: Sto,
      kon: KonA,
      t0: Timestamp,
      ext: Ext,
    ): (Env, Sto, Timestamp, Ext) =
      // split in fixed an variable number of arguments
      val (fx, vra) = argv.zip(fexp.args).splitAt(lam.args.length)
      val ctx = ()

      // add the fixed arguments on addresses in the store
      val (sto1, ext1) = lam.args.zip(fx).foldLeft((sto0, ext)) { case ((sto, ext), (par, (value, _))) =>
        writeStoV(sto, VarAddr(par.idn, ctx), value, ext)
      }
      // add variable arguments as a list to a particular address in the store
      val (sto2, ext2) = lam.varArgId match
          case Some(id) =>
            val (vlu, newsto) = allocList(vra, lex, sto1, kon, t0)
            writeStoV(newsto, VarAddr(id.idn, ctx), vlu, ext)
          case _ => (sto1, ext1)

      // extend the environment with the correct bindigs
      val pars = (lam.args ++ lam.varArgId.map(List(_)).getOrElse(List()))
      val env1 = pars.foldLeft(lex)((env, par) => env.extend(par.name, VarAddr(par.idn, ctx)))
      (env1, sto2, t0, ext2)

  protected def applyClo(
      fexp: SchemeFuncall,
      func: Val,
      argv: List[Val],
      env: Env,
      sto: Sto,
      kon: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    // TODO: introduce contexts to support things like k-cfa
    lattice.getClosures(project(func)).flatMap {
      case (lam, lex: Env @unchecked) if lam.check(argv.size) =>
        val (env1, sto2, t0, ext1) = bindArgs(fexp, argv, lam, lex, sto, kon, t, ext)
        val (sto3, frame, t1) = pushFrameRet(fexp, env, sto2, kon, EmptyFrame(), t)
        // and evaluate the body
        evaluate_sequence(env1, sto3, frame, lam.body, t1, ext1, true)
      case (lam, lex) =>
        invalidArity(fexp, argv.size, lam.args.size + lam.varArgId.size, sto, kon, t, ext)
    }

  /** Call to the given primitive while ingoring the changes to the store */
  protected def callPrimitive(
      fexp: SchemeExp,
      func: SchemePrimitive[LatVal, Address],
      argv: List[Val],
    ): LatVal =
      given bridge: InterpreterBridge = InterpreterBridge(initialEnv, initialStore, Kont0Addr, initialTime, emptyExt)
      func.callMF(fexp, argv.map(project)) match
          case MayFailSuccess(vlu) =>
            val sto1 = bridge.updatedSto
            vlu
          case MayFailBoth(vlu, _) =>
            val sto1 = bridge.updatedSto
            vlu
          // executing the primitive is unsuccessfull, no successors states are generated
          case MayFailError(_) => lattice.bottom

  /** Apply the given value as a primitive function (if the value is a primitive function) */
  protected def applyPrim(
      fexp: SchemeFuncall,
      func: Val,
      argv: List[Val],
      env: Env,
      sto: Sto,
      kon: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
    lattice.getPrimitives(project(func)).flatMap { name =>
        val primitive = primitives(name)
        given bridge: InterpreterBridge = InterpreterBridge(env, sto, kon, t, ext)
        primitive.callMF(fexp, argv.map(project)) match
            // the primitive is successfull apply the continuation with the value returned from the primitive
            case MayFailSuccess(vlu) =>
              val sto1 = bridge.updatedSto
              ap(inject(vlu), sto1, kon, t, ext)
            case MayFailBoth(vlu, _) =>
              val sto1 = bridge.updatedSto
              ap(inject(vlu), sto1, kon, t, ext)
            // executing the primitive is unsuccessfull, no successors states are generated
            case MayFailError(_) => Set()
    }

  protected def cond(
      value: Val,
      csq: Expr,
      alt: Expr,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp,
      ext: Ext
    ): Set[State] =
      import Control.*
      val csqSt = if lattice.isTrue(project(value)) then ev(csq, env, sto, kont, t, ext) else Set()
      val altSt = if lattice.isFalse(project(value)) then ev(alt, env, sto, kont, t, ext) else Set()
      csqSt ++ altSt

  private def allocList(items: List[(Val, SchemeExp)], env: Env, sto: Sto, kont: KonA, t: Timestamp): (Val, Sto) = items match
      case Nil => (inject(lattice.nil), sto)
      case (vlu, exp) :: rest =>
        val (tail, sto1) = allocList(rest, env, sto, kont, t)
        allocCons(exp, vlu, tail, env, sto1, kont, t)

  private def allocCons(
      exp: SchemeExp,
      car: Val,
      cdr: Val,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp
    ): (Val, Sto) =
      val addr = alloc(exp.idn, env, sto, kont, t) // TODO: check whether a seperate addr is needed for cons
      val sto1 = writeSto(sto, addr, Storable.V(lattice.cons(project(car), project(cdr))))
      (inject(lattice.pointer(addr)), sto1)

  protected def continueWith(sto: Sto, kont: KonA)(f: (KonA) => SchemeState): Set[State] =
    Set(f(kont))

  protected def continueWiths(sto: Sto, kont: KonA)(f: (KonA) => Set[SchemeState]): Set[State] =
    f(kont)

  /** From the given store get a set of continuations associated with  the given address */
  protected def readKonts(sto: Sto, kont: KonA): Set[Kont] = kont match
      case addr: Address =>
        readSto(sto, addr) match
            case Storable.K(ks) => ks
            case _              => Set()
      case frame: Kont => Set(frame)

  /** Return a value to the next continuation */
  protected def ap(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Set[State] =
    Set(SchemeState(Control.Ap(value), sto, kont, t, ext))

  /** Evaluate the given expression in the given environment */
  protected def ev(
      exp: SchemeExp,
      env: Env,
      sto: Sto,
      kont: KonA,
      t: Timestamp,
      ext: Ext,
      call: Boolean = false
    ): Set[State] =
    Set(SchemeState(Control.Ev(exp, env), sto, kont, t, ext))

  /** Apply the given continuation with the given value */
  def continue(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Set[State] =
    readKonts(sto, kont).flatMap {
      case EmptyFrame(Some(next)) =>
        continueWiths(sto, next)(ap(value, sto, _, t, ext))

      // (Ap(v), env, sto, assgn(x) :: k) ==> (Ap(nil), env, sto', k)
      //    where sto' = sto [ env(x) -> v ]
      case AssFrame(id, env, Some(next)) =>
        val sto1 = writeSto(sto, env.lookup(id.name).get, Storable.V(project(value)))
        continueWiths(sto, next)(ap(inject(lattice.nil), sto1, _, t, ext))

      // (Ap(v), env, sto, beg(e1 e2 ... en) :: k) ==> (Ev(e1), env, sto, beg(e2 .. en) :: k)
      case BegFrame(e1 :: exps, env, cross, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
            val (sto1, frame, t1) = pushFrame(e1, env, sto, kont, BegFrame(exps, env, cross), t)
            ev(e1, env, sto1, frame, t1, ext)
        }

      // (Ap(v), env, sto, beg() :: k) ==> (Ap(v), env, sto, k)
      case BegFrame(List(), env, cross, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
          ap(value, sto, kont, t, ext)
        }

      // (Ap(true), env, sto, ite(csq, alt) :: k) ==> (Ev(csq), env, sto, k)
      // (Ap(false), env, sto, ite(csq, alt) :: k) ==> (Ev(alt), env, sto, k)
      case IteFrame(csq, alt, env, Some(addr)) =>
        continueWiths(sto, addr)(cond(value, csq, alt, env, sto, _, t, ext))

      // (Ap(fv), env, sto, fun(f, a :: args) :: k) ==> (Ev(a), env, sto, FunArg(f, args, fv, List()) :: k)
      case FunFrame(f, arg :: args, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
            val (sto1, frame, t1) = pushFrame(arg, env, sto, kont, ArgFrame(f, args, value, List(), env), t)
            ev(arg, env, sto1, frame, t1, ext)
        }

      // (Ap(fv), env, sto, fun(f, ()) :: k) ==> (Ev(a), env, sto, ret(env) :: k)
      case FunFrame(f, List(), env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
          applyFun(f, value, List(), env, sto, kont, t, ext)
        }

      case ArgFrame(f, arg :: args, fv, argsV, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
            val (sto1, frame, t1) = pushFrame(arg, env, sto, kont, ArgFrame(f, args, fv, value :: argsV, env), t)
            ev(arg, env, sto1, frame, t1, ext)
        }

      case ArgFrame(f, List(), fv, argsV, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
          applyFun(f, fv, (value :: argsV).reverse, env, sto, kont, t, ext)
        }

      case LetFrame(evalBds, binding :: bindings, body, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
          evaluateLet((binding._1, value) :: evalBds, env, sto, kont, bindings, body, t, ext)
        }

      case LetStarFrame(currentIdentifier, restBindings, body, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
            val addr = alloc(currentIdentifier.idn, env, sto, kont, t)
            val env1 = env.extend(currentIdentifier.name, addr)
            val (sto1, ext1) = writeStoV(sto, addr, value, ext)
            evaluateLetStar(env1, sto1, kont, restBindings, body, t, ext1)
        }

      case LetrecFrame(currentAddr, addresses, values, bindings, body, env, Some(addr)) =>
        continueWiths(sto, addr) { kont =>
            val (sto1, ext1) = writeStoV(sto, currentAddr, value, ext)
            evaluateLetrec(addresses, value :: values, env, sto1, kont, bindings, body, t, ext1)
        }

      case AssertFrame(idn, env, Some(next)) =>
        if !lattice.isTrue(project(value)) then error(AssertionFailed(idn), sto, next, t, ext)
        else ap(inject(lattice.nil), sto, next, t, ext)

      case HltFrame => Set()
    }
}

abstract class SchemeAAMSemantics(b: SchemeExp) extends BaseSchemeAAMSemantics(b)
