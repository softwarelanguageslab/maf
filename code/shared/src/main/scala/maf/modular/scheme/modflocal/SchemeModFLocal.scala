package maf.modular.scheme.modflocal

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

abstract class SchemeModFLocal(prog: SchemeExp) extends ModAnalysis[SchemeExp](prog) with SchemeDomain {
  this: SchemeModFLocalSensitivity =>

  // shorthands
  type Val = Value
  type Adr = Address
  type Exp = SchemeExp
  type Lam = SchemeLambdaExp
  type Sto = LocalStore[Adr, Storable]
  type Kon = List[Frame]
  type Clo = (Lam, Env)
  type Env = NestedEnv[Address, EnvAddr] // TODO: NestedEnv[VarAddr[Ctx], EnvAddr]
  type Idn = Identity
  type Pos = Position
  type Cmp = Component

  lazy val initialExp: Exp = program
  lazy val initialEnv: Env = NestedEnv(initialBds.map(p => (p._1, p._2)).toMap, None)
  lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }

  lazy val initialBds: Iterable[(String, Adr, Storable)] =
    primitives.allPrimitives.map { case (name, p) =>
      (name, PrmAddr(name), V(lattice.primitive(p.name)))
    }

  // the store is used for several purposes:
  // - mapping variable/pointer addresses to values
  // - mapping environment addresses to environments
  // - mapping continuation addresses to continuations

  sealed trait Storable
  case class V(vlu: Value) extends Storable
  case class E(evs: Set[Env]) extends Storable
  case class K(kts: Set[(Kon, Ctx)]) extends Storable

  implicit def storableLattice: LatticeWithAddrs[Storable, Adr] = new LatticeWithAddrs[Storable, Adr] {
    def bottom: Storable = throw new Exception("No single bottom element in Storable lattice")
    override def isBottom(x: Storable) = x match {
      case V(vlu) => vlu == lattice.bottom
      case E(evs) => evs.isEmpty
      case K(kts) => kts.isEmpty
    }
    def join(x: Storable, y: => Storable): Storable = (x, y) match {
      case (V(v1), V(v2)) => V(lattice.join(v1, v2))
      case (E(e1), E(e2)) => E(e1 ++ e2)
      case (K(k1), K(k2)) => K(k1 ++ k2)
      case _              => throw new Exception(s"Attempting to join incompatible elements $x and $y")
    }
    def refs(x: Storable): Set[Adr] = x match {
      case V(vlu) => lattice.refs(vlu)
      case E(evs) => evs.flatMap(refsEnv)
      case K(kts) => kts.flatMap { case (kon, _) => kon.flatMap(refsFrm) }
    }
    def top: Storable = throw new Exception("No top element in Storable lattice")
    def subsumes(x: Storable, y: => Storable): Boolean = throw new Exception("NYI")
    def eql[B: BoolLattice](x: Storable, y: Storable): B = throw new Exception("NYI")
    def show(v: Storable): String = v.toString
  }

  case class EnvAddr(lam: Lam, ctx: Ctx) extends Address {
    def idn = lam.idn
    def printable = true
    override def toString = s"EnvAddr(${lam.lambdaName}, $ctx)"
  }
  case class KonAddr(lam: Lam, ctx: Ctx) extends Address {
    def idn = lam.idn
    def printable = true
    override def toString = s"KonAddr(${lam.lambdaName}, $ctx)"
  }
  case class ResAddr(kon: Kon, ctx: Ctx) extends Address {
    def idn = Identity.none
    def printable = true
    override def toString = s"ResAddr($kon, $ctx)"
  }
  case class FrmAddr(exp: Exp, ctx: Ctx) extends Address {
    def idn = exp.idn
    def printable = true
    override def toString = s"FrmAddr($exp, $ctx)"
  }

  // continuations are a list of frames
  // the last frame is either a fn return or a halt
  sealed trait Frame
  case object HltFrame extends Frame
  case class RetFrame(adr: KonAddr) extends Frame
  case class SeqFrame(eps: List[Exp], env: Env) extends Frame
  case class IteFrame(
      csq: Exp,
      alt: Exp,
      env: Env)
      extends Frame
  case class AssFrame(id: Identifier, env: Env) extends Frame
  case class FunFrame(
      fun: SchemeFuncall,
      ags: List[Exp],
      env: Env)
      extends Frame
  case class ArgFrame(
      fun: SchemeFuncall,
      fad: Adr,
      aad: List[(Exp, Adr)],
      ags: List[Exp],
      env: Env)
      extends Frame
  case class LetFrame(
      bdy: List[Exp],
      bad: List[(Identifier, Adr)],
      bds: List[(Identifier, Exp)],
      env: Env)
      extends Frame
  case class LttFrame(
      bdy: List[Exp],
      bds: List[(Identifier, Exp)],
      env: Env)
      extends Frame
  case class LtrFrame(
      bdy: List[Exp],
      bds: List[(Identifier, Exp)],
      env: Env)
      extends Frame
  case class AndFrame(rst: List[Exp], env: Env) extends Frame
  case class OrrFrame(rst: List[Exp], env: Env) extends Frame
  case class PcaFrame(pai: SchemePair, env: Env) extends Frame
  case class PcdFrame(pai: SchemePair, car: FrmAddr) extends Frame
  case class ScaFrame(pai: SchemeSplicedPair, env: Env) extends Frame
  case class ScdFrame(pai: SchemeSplicedPair, car: FrmAddr) extends Frame

  // components: either a main/call component, or a continuation call
  sealed trait Component extends Serializable { def ctx: Ctx }
  case object MainComponent extends Component {
    def ctx = initialCtx
    override def toString = "main"
  }
  case class CallComponent(
      lam: Lam,
      ctx: Ctx,
      sto: Sto)
      extends Component {
    override def toString = s"${lam.lambdaName} [$ctx]"
  }
  case class KontComponent(
      kon: Kon,
      ctx: Ctx,
      sto: Sto)
      extends Component {
    override def toString = s"<continuation> [$ctx]" //TODO
  }
  case class HaltComponent(vlu: Val, sto: Sto) extends Component {
    def ctx = initialCtx
    override def toString = s"HALT($vlu)"
  }

  private def gc(cmp: Component): Component = cmp match {
    case MainComponent => cmp
    case CallComponent(lam, ctx, sto) =>
      val rs = lam.args.map(VarAddr(_, ctx)).toSet[Adr] ++
        lam.varArgId.map(VarAddr(_, ctx)).toSet[Adr] +
        EnvAddr(lam, ctx) +
        KonAddr(lam, ctx)
      CallComponent(lam, ctx, sto.collect(rs))
    case KontComponent(kon, ctx, sto) =>
      val rs = kon.flatMap(refsFrm).toSet + ResAddr(kon, ctx)
      KontComponent(kon, ctx, sto.collect(rs))
    case HaltComponent(vlu, sto) =>
      val rs = lattice.refs(vlu)
      HaltComponent(vlu, sto.collect(rs))
  }

  // GC every component that is spawned
  override def spawn(cmp: Component) = super.spawn(gc(cmp))

  private def refsEnv(e: Env): Set[Adr] = e.addrs
  private def refsFrm(frm: Frame): Set[Adr] = frm match {
    case HltFrame                      => Set.empty
    case RetFrame(adr)                 => Set(adr)
    case AndFrame(_, env)              => refsEnv(env)
    case ArgFrame(_, fad, aad, _, env) => refsEnv(env) ++ aad.map(_._2) + fad
    case AssFrame(_, env)              => refsEnv(env)
    case FunFrame(_, _, env)           => refsEnv(env)
    case IteFrame(_, _, env)           => refsEnv(env)
    case LetFrame(_, bad, _, env)      => refsEnv(env) ++ bad.map(_._2)
    case LtrFrame(_, _, env)           => refsEnv(env)
    case LttFrame(_, _, env)           => refsEnv(env)
    case OrrFrame(_, env)              => refsEnv(env)
    case PcaFrame(_, env)              => refsEnv(env)
    case PcdFrame(_, _)                => Set.empty
    case ScaFrame(_, env)              => refsEnv(env)
    case ScdFrame(_, _)                => Set.empty
    case SeqFrame(_, env)              => refsEnv(env)
  }

  def initialComponent: Component = MainComponent
  def expr(cmp: Component): Exp = cmp match {
    case MainComponent            => program
    case CallComponent(lam, _, _) => lam
    case KontComponent(_, _, _)   => SchemeVar(Identifier("kont", Identity.none)) // TODO
    case HaltComponent(_, _)      => SchemeVar(Identifier("done", Identity.none))
  }

  override def intraAnalysis(cmp: Component) = new SchemeIntraAnalysis(cmp)
  class SchemeIntraAnalysis(cmp: Component) extends IntraAnalysis(cmp) {

    def analyzeWithTimeout(timeout: Timeout.T): Unit = cmp match {
      case HaltComponent(_, _) => ()
      case MainComponent =>
        eval(initialExp, initialEnv, initialSto, List(HltFrame))
      case CallComponent(lam, ctx, sto) =>
        val fixedArgEnv = lam.args.map(par => (par.name, VarAddr(par, ctx))).toMap
        val varArgEnv = lam.varArgId.map(par => (par.name, (VarAddr(par, ctx)))).toMap
        val env: NestedEnv[Address, EnvAddr] = NestedEnv(fixedArgEnv ++ varArgEnv, Some(EnvAddr(lam, ctx)))
        evalSequence(lam.body, env, sto, List(RetFrame(KonAddr(lam, ctx))))
      case KontComponent(kon, ctx, sto) =>
        continue(kon, lookupV(sto, ResAddr(kon, ctx)), sto)
    }

    // EVAL

    private def eval(
        exp: Exp,
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = exp match {
      case lit: SchemeValue =>
        evalLiteralValue(lit, sto, kon)
      case lam: SchemeLambdaExp =>
        continue(kon, lattice.closure((lam, env.restrictTo(lam.fv))), sto)
      case SchemeVar(id) =>
        evalVariable(id, env, sto, kon)
      case SchemeSet(id, ep0, _) =>
        eval(ep0, env, sto, AssFrame(id, env) :: kon)
      case SchemeBegin(eps, _) =>
        evalSequence(eps, env, sto, kon)
      case SchemeIf(prd, csq, alt, _) =>
        eval(prd, env, sto, IteFrame(csq, alt, env) :: kon)
      case fun @ SchemeFuncall(f, args, _) =>
        eval(f, env, sto, FunFrame(fun, args, env) :: kon)
      case SchemeLet(bds, bdy, _) =>
        evalLet(Nil, bds, bdy, env, sto, kon)
      case SchemeLetStar(bds, bdy, _) =>
        evalLetStar(bds, bdy, env, sto, kon)
      case SchemeLetrec(bds, bdy, _) =>
        val adrs = bds.map { case (idf, _) => (idf.name, VarAddr(idf, cmp.ctx)) }
        val env1 = env.extend(adrs)
        evalLetrec(bds, bdy, env1, sto, kon)
      case SchemeNamedLet(id, bds, bdy, idn) =>
        val (prs, ags) = bds.unzip
        val lam = SchemeLambda(Some(id.name), prs, bdy, idn)
        val adr = VarAddr(id, cmp.ctx)
        val lex = env.extend(id.name, adr)
        val clo = lattice.closure((lam, lex.restrictTo(lam.fv)))
        val sto1 = extendV(sto, adr, clo)
        val call = SchemeFuncall(lam, ags, idn)
        evalArgs(call, adr, Nil, ags, env, sto1, kon)
      case SchemeAnd(Nil, _) =>
        continue(kon, lattice.bool(true), sto)
      case SchemeAnd(ep0 :: eps, _) =>
        evalAnd(ep0, eps, env, sto, kon)
      case SchemeOr(eps, _) =>
        evalOr(eps, env, sto, kon)
      case pai: SchemePair =>
        eval(pai.car, env, sto, PcaFrame(pai, env) :: kon)
      case spi: SchemeSplicedPair =>
        eval(spi.splice, env, sto, ScaFrame(spi, env) :: kon)
      case _: SchemeAssert =>
        continue(kon, lattice.void, sto)
      case _ =>
        throw new Exception(s"Unsupported expression: $exp")
    }

    private def evalLiteralValue(
        exp: SchemeValue,
        sto: Sto,
        kon: Kon
      ): Unit = exp.value match {
      case sexp.Value.String(s) =>
        val (sptr, sto1) = allocateVal(exp, sto, lattice.string(s))
        continue(kon, sptr, sto1)
      case sexp.Value.Integer(n)   => continue(kon, lattice.number(n), sto)
      case sexp.Value.Real(r)      => continue(kon, lattice.real(r), sto)
      case sexp.Value.Boolean(b)   => continue(kon, lattice.bool(b), sto)
      case sexp.Value.Character(c) => continue(kon, lattice.char(c), sto)
      case sexp.Value.Symbol(s)    => continue(kon, lattice.symbol(s), sto)
      case sexp.Value.Nil          => continue(kon, lattice.nil, sto)
      case lit                     => throw new Exception(s"Unsupported Scheme literal: $lit")
    }

    private def lookupVariable(
        id: Identifier,
        env: Env,
        sto: Sto
      )(
        clb: Address => Unit
      ): Unit =
      env.lookup(id.name) match {
        case Some(addr) => clb(addr)
        case None if env.rst.isDefined =>
          lookupE(sto, env.rst.get).foreach(lookupVariable(id, _, sto)(clb))
        case None => throw new Exception(s"Undefined variable: ${id.name}")
      }

    private def evalVariable(
        id: Identifier,
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit =
      lookupVariable(id, env, sto) { addr => continue(kon, lookupV(sto, addr), sto) }

    private def assignVariable(
        id: Identifier,
        env: Env,
        sto: Sto,
        vlu: Val,
        kon: Kon
      ): Unit =
      lookupVariable(id, env, sto) { addr =>
        val sto1 = updateV(sto, addr, vlu)
        continue(kon, lattice.void, sto1)
      }

    private def evalLet(
        bad: List[(Identifier, Adr)],
        bds: List[(Identifier, Exp)],
        bdy: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = bds match {
      case Nil =>
        val (env1, sto1) = bad.reverse.foldLeft((env, sto)) { case (acc, (idf, frm)) =>
          val vlu = lookupV(sto, frm)
          val adr = VarAddr(idf, cmp.ctx)
          (acc._1.extend(idf.name, adr), extendV(acc._2, adr, vlu))
        }
        evalSequence(bdy, env1, sto1, kon)
      case (_, rhs) :: _ =>
        eval(rhs, env, sto, LetFrame(bdy, bad, bds, env) :: kon)
    }

    private def evalLetStar(
        bds: List[(Identifier, Exp)],
        bdy: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = bds match {
      case Nil           => evalSequence(bdy, env, sto, kon)
      case (_, rhs) :: _ => eval(rhs, env, sto, LttFrame(bdy, bds, env) :: kon)
    }

    private def evalLetrec(
        bds: List[(Identifier, Exp)],
        bdy: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = bds match {
      case Nil =>
        evalSequence(bdy, env, sto, kon)
      case (_, rhs) :: _ =>
        eval(rhs, env, sto, LtrFrame(bdy, bds, env) :: kon)
    }

    private def evalSequence(
        eps: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = eps match {
      case Nil        => continue(kon, lattice.void, sto)
      case lst :: Nil => eval(lst, env, sto, kon)
      case ep0 :: rst => eval(ep0, env, sto, SeqFrame(rst, env) :: kon)
    }

    private def evalArgs(
        fun: SchemeFuncall,
        fad: Adr,
        aad: List[(Exp, Adr)],
        ags: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ): Unit = ags match {
      case Nil =>
        val funVal = lookupV(sto, fad)
        val argVals = aad.map { case (idf, adr) => (idf, lookupV(sto, adr)) }
        applyProc(fun, funVal, argVals.reverse, sto, kon)
      case arg :: _ =>
        val frm = ArgFrame(fun, fad, aad, ags, env)
        eval(arg, env, sto, frm :: kon)
    }

    private def applyProc(
        fun: SchemeFuncall,
        fvl: Val,
        ags: List[(Exp, Val)],
        sto: Sto,
        kon: Kon
      ): Unit = {
      applyPrimitives(fun, fvl, ags, sto, kon)
      applyClosures(fun, fvl, ags, sto, kon)
    }

    private def applyPrimitives(
        fun: SchemeFuncall,
        fvl: Val,
        ags: List[(Exp, Val)],
        sto: Sto,
        kon: Kon
      ): Unit =
      lattice.getPrimitives(fvl).foreach { prm =>
        // TODO: non-deterministic control-flow from primitives
        primitives(prm).call(fun, ags, StoreAdapter(sto), InterpreterBridge(cmp.ctx)) match {
          case MayFailSuccess((vlu, adp)) => continue(kon, vlu, adp.sto)
          case MayFailError(_)            => ()
          case MayFailBoth((vlu, adp), _) => continue(kon, vlu, adp.sto)
        }
      }

    private def applyClosures(
        fun: SchemeFuncall,
        fvl: Val,
        ags: List[(Exp, Val)],
        sto: Sto,
        kon: Kon
      ): Unit = {
      val pos = fun.idn.pos
      val agc = ags.length
      lattice.getClosures(fvl).foreach {
        case (lam, lex: Env @unchecked) if lam.check(agc) =>
          val (fxa, vra) = ags.splitAt(lam.args.length)
          val cctx = allocCtx(lam, lex, ags, pos, cmp)
          val sto1 = lam.args.zip(fxa).foldLeft(sto) { case (acc, (par, (_, arg))) =>
            extendV(acc, VarAddr(par, cctx), arg)
          }
          val sto2 = if (lam.varArgId.isDefined) {
            val (lst, sto2a) = allocList(vra, sto1)
            extendV(sto2a, VarAddr(lam.varArgId.get, cctx), lst)
          } else {
            sto1
          }
          val sto3 = extendE(sto2, EnvAddr(lam, cctx), Set(lex))
          val sto4 = extendK(sto3,
                             KonAddr(lam, cctx),
                             kon match {
                               case RetFrame(kad) :: Nil => lookupK(sto, kad)
                               case _                    => Set((kon, cmp.ctx))
                             }
          )
          spawn(CallComponent(lam, cctx, sto4))
        case _ => ()
      }
    }

    private def evalAnd(
        nxt: Exp,
        rst: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ) =
      if (rst.isEmpty) {
        eval(nxt, env, sto, kon)
      } else {
        eval(nxt, env, sto, AndFrame(rst, env) :: kon)
      }

    private def evalOr(
        eps: List[Exp],
        env: Env,
        sto: Sto,
        kon: Kon
      ) = eps match {
      case Nil        => continue(kon, lattice.bool(false), sto)
      case ep0 :: eps => eval(ep0, env, sto, OrrFrame(eps, env) :: kon)
    }

    protected def allocList(els: List[(Exp, Val)], sto: Sto): (Val, Sto) = els match {
      case Nil => (lattice.nil, sto)
      case (exp, head) :: rst =>
        val (tail, sto1) = allocList(rst, sto)
        allocCons(exp, sto1, head, tail)
    }
    protected def allocCons(
        pairExp: Exp,
        sto: Sto,
        car: Val,
        cdr: Val
      ): (Val, Sto) =
      allocateVal(pairExp, sto, lattice.cons(car, cdr))
    protected def allocateVal(
        exp: Exp,
        sto: Sto,
        vlu: Val
      ): (Val, Sto) = {
      val addr = PtrAddr(exp, cmp.ctx)
      val sto1 = extendV(sto, addr, vlu)
      (lattice.pointer(addr), sto1)
    }

    // CONTINUE

    private def continue(
        kon: Kon,
        vlu: Val,
        sto: Sto
      ): Unit =
      if (!lattice.isBottom(vlu)) {
        kon match {
          // inter-procedural continuations
          case HltFrame :: Nil =>
            spawn(HaltComponent(vlu, sto))
          case RetFrame(adr) :: Nil =>
            lookupK(sto, adr).foreach { case (kon, ctx) =>
              val addr = ResAddr(kon, ctx)
              val sto1 = extendV(sto, addr, vlu)
              spawn(KontComponent(kon, ctx, sto1))
            }
          // local continuations
          case SeqFrame(eps, env) :: rst =>
            evalSequence(eps, env, sto, rst)
          case IteFrame(csq, alt, env) :: rst =>
            if (lattice.isTrue(vlu)) { eval(csq, env, sto, rst) }
            if (lattice.isFalse(vlu)) { eval(alt, env, sto, rst) }
          case AssFrame(id, env) :: rst =>
            assignVariable(id, env, sto, vlu, rst)
          case FunFrame(fun, args, env) :: rst =>
            val addr = FrmAddr(fun.f, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            evalArgs(fun, addr, Nil, args, env, sto1, rst)
          case ArgFrame(fun, fad, aad, ag0 :: agr, env) :: rst =>
            val addr = FrmAddr(ag0, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            evalArgs(fun, fad, (ag0, addr) :: aad, agr, env, sto1, rst)
          case LetFrame(bdy, bad, (idf, rhs) :: bds, env) :: rst =>
            val addr = FrmAddr(rhs, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            evalLet((idf, addr) :: bad, bds, bdy, env, sto1, rst)
          case LttFrame(bdy, (idf, _) :: bds, env) :: rst =>
            val addr = VarAddr(idf, cmp.ctx)
            val env1 = env.extend(idf.name, addr)
            val sto1 = extendV(sto, addr, vlu)
            evalLetStar(bds, bdy, env1, sto1, rst)
          case LtrFrame(bdy, (idf, _) :: bds, env) :: rst =>
            val addr = VarAddr(idf, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            evalLetrec(bds, bdy, env, sto1, rst)
          case AndFrame(nxt :: oth, env) :: rst =>
            if (lattice.isTrue(vlu)) { evalAnd(nxt, oth, env, sto, rst) }
            if (lattice.isFalse(vlu)) { continue(rst, lattice.bool(false), sto) }
          case OrrFrame(oth, env) :: rst =>
            if (lattice.isTrue(vlu)) { continue(rst, vlu, sto) }
            if (lattice.isFalse(vlu)) { evalOr(oth, env, sto, rst) }
          case PcaFrame(pai, env) :: rst =>
            val addr = FrmAddr(pai.car, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            eval(pai.cdr, env, sto1, PcdFrame(pai, addr) :: rst)
          case PcdFrame(pai, frm) :: rst =>
            val car = lookupV(sto, frm)
            val (res, sto1) = allocCons(pai, sto, car, vlu)
            continue(rst, res, sto1)
          case ScaFrame(spi, env) :: rst =>
            val addr = FrmAddr(spi.splice, cmp.ctx)
            val sto1 = extendV(sto, addr, vlu)
            eval(spi.cdr, env, sto1, ScdFrame(spi, addr) :: rst)
          case ScdFrame(_, _) :: rst =>
            //val spl = lookupV(sto, frm)
            val (res, sto1): (Val, Sto) = ??? // NYI -- append
            continue(rst, res, sto1)
          case _ =>
            throw new Exception(s"Unsupported continuation $kon")
        }
      }
  }

  // STORE HELPERS

  private def lookupK(sto: Sto, adr: KonAddr): Set[(Kon, Ctx)] =
    sto.lookup(adr) match {
      case Some(K(kns)) => kns
      case _            => throw new Exception("This should not happen")
    }

  private def lookupE(sto: Sto, adr: EnvAddr): Set[Env] =
    sto.lookup(adr) match {
      case Some(E(env)) => env
      case _            => throw new Exception("This should not happen")
    }

  private def lookupV(sto: Sto, adr: Adr): Value =
    sto.lookup(adr) match {
      case Some(V(vlu)) => vlu
      case x            => throw new Exception(s"This should not happen ($adr -> $x)")
    }

  protected def extendV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.extend(adr, V(vlu))
  protected def updateV(sto: Sto, adr: Adr, vlu: Val): Sto = sto.update(adr, V(vlu))
  protected def extendE(sto: Sto, adr: Adr, evs: Set[Env]): Sto = sto.extend(adr, E(evs))
  protected def extendK(sto: Sto, adr: Adr, kts: Set[(Kon, Ctx)]): Sto = sto.extend(adr, K(kts))

  case class StoreAdapter(sto: Sto) extends Store[Adr, Val] {
    type This = StoreAdapter
    def lookup(adr: Adr) = sto.lookup(adr) match {
      case Some(V(vlu)) => Some(vlu)
      case _            => None
    }
    def extend(adr: Adr, vlu: Val) = this.copy(sto = extendV(sto, adr, vlu))
    override def update(adr: Adr, vlu: Val) = this.copy(sto = sto.update(adr, V(vlu)))
    // join operations -- these are currently not actually used
    def empty = StoreAdapter(sto.empty)
    def join(other: StoreAdapter) = StoreAdapter(sto.join(other.sto))
  }

  case class InterpreterBridge(ctx: Ctx) extends SchemeInterpreterBridge[Val, Adr] {
    def pointer(exp: SchemeExp): Adr = PtrAddr(exp, ctx)
    def callcc(clo: lattice.Closure, pos: Position): Value = throw new Exception("NYI")
    def currentThread: TID = throw new Exception("NYI")
  }
}
