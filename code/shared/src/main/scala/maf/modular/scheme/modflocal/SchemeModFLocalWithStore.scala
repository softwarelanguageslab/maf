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

//
// BIG-STEP
//

abstract class SchemeModFWithStore(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeDomain {
  this: SchemeModFWithStoreSensitivity =>

  // shorthands
  type Val = Value
  type Adr = Address
  type Exp = SchemeExp
  type Lam = SchemeLambdaExp
  type Sto = LocalStore[Adr, Storable] // TODO: split the store
  type Clo = (Lam, Env)
  type Env = NestedEnv[Adr, EnvAddr] // TODO: NestedEnv[VarAddr[Ctx], EnvAddr]
  type Idn = Identity
  type Pos = Position
  type Cmp = Component

  // parameterised by widening functions
  // by default, there is no widening
  def widen(res: Set[(Val, Sto)]): Set[(Val, Sto)] = res

  // AnalysisM monad
  case class AnalysisM[X](run: (Ctx, Env, Sto) => Set[(X, Sto)]) {
    def flatMap[Y](f: X => AnalysisM[Y]): AnalysisM[Y] =
      AnalysisM((ctx, env, sto) => run(ctx, env, sto).flatMap(res => f(res._1).run(ctx, env, res._2)))
    def map[Y](f: X => Y): AnalysisM[Y] =
      AnalysisM((ctx, env, sto) => run(ctx, env, sto).map(res => (f(res._1), res._2)))
    def withFilter(p: X => Boolean): AnalysisM[X] = 
      AnalysisM((ctx, env, sto) => run(ctx, env, sto).filter(res => p(res._1)))
    def ++(other: AnalysisM[X]): AnalysisM[X] = mplus(this, other)
  }
  def unit[X](x: X): AnalysisM[X] = AnalysisM((_, _, sto) => Set((x, sto)))
  def pick[X](xs: Set[X]): AnalysisM[X] = AnalysisM((_, _, sto) => xs.map((_, sto)))
  def mzero[X]: AnalysisM[X] = AnalysisM((_, _, _) => Set.empty[(X, Sto)])
  def mplus[X](x: AnalysisM[X], y: AnalysisM[X]): AnalysisM[X] =
    AnalysisM((ctx, env, sto) => x.run(ctx, env, sto) ++ y.run(ctx, env, sto))
  def mplus[X](xs: Iterable[AnalysisM[X]]): AnalysisM[X] =
    xs.foldLeft[AnalysisM[X]](mzero)((acc, x) => mplus(acc, x))
  def guard(cnd: Boolean): AnalysisM[Unit] =
    if (cnd) unit(()) else mzero
  def inject[X: Lattice](x: X): AnalysisM[X] =
    guard(!Lattice[X].isBottom(x)).flatMap(_ => unit(x))
  implicit class MonadicOps[X](xs: Iterable[X]) {
    def foldLeftM[Y](y: Y)(f: (Y, X) => AnalysisM[Y]): AnalysisM[Y] = xs match {
      case Nil     => unit(y)
      case x :: xs => f(y, x).flatMap(acc => xs.foldLeftM(acc)(f))
    }
    def mapM[Y](f: X => AnalysisM[Y]): AnalysisM[List[Y]] = xs match {
      case Nil => unit(Nil)
      case x :: xs =>
        for {
          fx <- f(x)
          rest <- xs.mapM(f)
        } yield fx :: rest
    }
    def mapM_(f: X => AnalysisM[Unit]): AnalysisM[Unit] = xs match {
      case Nil     => unit(())
      case x :: xs => f(x).flatMap(_ => xs.mapM_(f))
    }
  }
	// env ops
  private def getEnv: AnalysisM[Env] = AnalysisM((_, env, sto) => Set((env, sto)))
	private def withExtendedEnv[X](nam: String, adr: VarAddr[Ctx])(blk: AnalysisM[X]): AnalysisM[X] =
		AnalysisM((ctx, env, sto) => blk.run(ctx, env.extend(nam, adr), sto))
	private def withExtendedEnv[X](bds: Iterable[(String,VarAddr[Ctx])])(blk: AnalysisM[X]): AnalysisM[X] =
		AnalysisM((ctx, env, sto) => blk.run(ctx, env.extend(bds), sto))
	private def lookupEnv(nam: String): AnalysisM[VarAddr[Ctx]] =
		AnalysisM((ctx, env, sto) => env.lookup(nam) match {
			case Some(adr: VarAddr[Ctx] @unchecked) => Set((adr, sto))
			case None if env.rst.isDefined =>
				sto(env.rst.get).asInstanceOf[E].evs.flatMap { nxt =>
					lookupEnv(nam).run(ctx, nxt, sto)
				}
		})
	// context ops
	private def getCtx: AnalysisM[Ctx] = AnalysisM((ctx, _, sto) => Set((ctx, sto)))
	private def allocPtr(exp: Exp): AnalysisM[PtrAddr[Ctx]] =
		for { ctx <- getCtx } yield PtrAddr(exp, ctx)
	private def allocVar(vrb: Identifier): AnalysisM[VarAddr[Ctx]] =
		for { ctx <- getCtx } yield VarAddr(vrb, ctx)
	// store ops
  private def getSto: AnalysisM[Sto] =
    AnalysisM((_, env, sto) => Set((sto, sto))) 
  private def setSto(sto: Sto): AnalysisM[Unit] =
    AnalysisM((_, env, _) => Set(((), sto)))
	protected def extendSto(adr: EnvAddr, evs: Set[Env]): AnalysisM[Unit] = 
    AnalysisM((_, env, sto) => Set(((), extendE(sto, adr, evs))))
	protected def extendSto(adr: SchemeAddr[Ctx], vlu: Val): AnalysisM[Unit] =
    AnalysisM((_, env, sto) => Set(((), extendV(sto, adr, vlu))))
  protected def extendSto(bds: Iterable[(SchemeAddr[Ctx], Val)]): AnalysisM[Unit] = bds.mapM_ { case (adr, vlu) => extendSto(adr, vlu) }
	protected def updateSto(adr: SchemeAddr[Ctx], vlu: Val): AnalysisM[Unit] =  //TODO: GC after an update!
    AnalysisM((_, env, sto) => Set(((), updateV(sto, adr, vlu))))
	protected def lookupSto(adr: SchemeAddr[Ctx]): AnalysisM[Val] = lookupSto(adr).map(_.asInstanceOf[V].vlu)
  protected def widen(x: AnalysisM[Val]): AnalysisM[Val] =
    AnalysisM { (ctx, env, sto) => widen(x.run(ctx, env, sto)) }

  // TODO: GC these?
  lazy val initialExp: Exp = program
  lazy val initialEnv: Env = NestedEnv(initialBds.map(p => (p._1, p._2)).toMap, None)
  lazy val initialSto: Sto = LocalStore.from(initialBds.map(p => (p._2, p._3)))

  override lazy val program: SchemeExp = {
    val originalProgram = super.program
    val preludedProgram = SchemePrelude.addPrelude(originalProgram)
    CSchemeUndefiner.undefine(List(preludedProgram))
  }

  private lazy val initialBds: Iterable[(String, Adr, Storable)] =
    primitives.allPrimitives.view
      .filterKeys(initialExp.fv)
      .map { case (name, p) =>
        (name, PrmAddr(name), V(lattice.primitive(p.name)))
      }

  // the store is used for several purposes:
  // - mapping variable/pointer addresses to values
  // - mapping environment addresses to environments
  sealed trait Storable
  case class V(vlu: Value) extends Storable
  case class E(evs: Set[Env]) extends Storable

  case class EnvAddr(lam: Lam, ctx: Ctx) extends Address {
    def idn = lam.idn
    def printable = true
    override def toString = s"EnvAddr(${lam.lambdaName}, $ctx)"
  }

  implicit def storableLattice: LatticeWithAddrs[Storable, Adr] = new LatticeWithAddrs[Storable, Adr] {
    def bottom: Storable = throw new Exception("No single bottom element in Storable lattice")
    override def isBottom(x: Storable) = x match {
      case V(vlu) => vlu == lattice.bottom
      case E(evs) => evs.isEmpty
    }
    def join(x: Storable, y: => Storable): Storable = (x, y) match {
      case (V(v1), V(v2)) => V(lattice.join(v1, v2))
      case (E(e1), E(e2)) => E(e1 ++ e2)
      case _              => throw new Exception(s"Attempting to join incompatible elements $x and $y")
    }
    def refs(x: Storable): Set[Adr] = x match {
      case V(vlu) => lattice.refs(vlu)
      case E(evs) => evs.flatMap(_.addrs)
    }
    def top: Storable = throw new Exception("No top element in Storable lattice")
    def subsumes(x: Storable, y: => Storable): Boolean = throw new Exception("NYI")
    def eql[B: BoolLattice](x: Storable, y: Storable): B = throw new Exception("NYI")
    def show(v: Storable): String = v.toString
  }

  sealed trait Component extends Serializable {
    def exp: Exp
    def env: Env
    def sto: Sto
    def ctx: Ctx
  }
  case object MainComponent extends Component {
    def exp = initialExp
    def env = initialEnv
    def sto = initialSto
    def ctx = initialCtx
    override def toString = "main"
  }
  case class CallComponent(lam: Lam, ctx: Ctx, sto: Sto) extends Component {
    def exp = SchemeBody(lam.body)
    def env = {
      val fixedArgEnv = lam.args.map(par => (par.name, VarAddr(par, ctx))).toMap
      val varArgEnv = lam.varArgId.map(par => (par.name, (VarAddr(par, ctx)))).toMap
      NestedEnv(fixedArgEnv ++ varArgEnv, Some(EnvAddr(lam, ctx)))
    }
    override def toString = s"${lam.lambdaName} [$ctx] [$sto]"
  }

  // results
  var results: Map[Component, Set[(Val, Sto)]] = Map.empty
  def addResult(cmp: Component, res: Set[(Val, Sto)]) =
    results += cmp -> (getResult(cmp) ++ res)
  def getResult(cmp: Component): Set[(Val, Sto)] =
    results.getOrElse(cmp, Set.empty)

  private def eval(exp: SchemeExp): AnalysisM[Val] = exp match {
    case vlu: SchemeValue             => evalLiteralValue(vlu)
    case lam: SchemeLambdaExp         => evalLambda(lam)
    case SchemeVar(id)                => evalVariable(id.name)
		case SchemeSet(id, rhs, _)        => evalSet(id.name, rhs)
		case SchemeBegin(eps, _)          => evalSequence(eps)
    case SchemeIf(prd, thn, alt, _)   => evalIf(prd,thn,alt)
    case SchemeLet(bds, bdy, _)       => evalLet(bds, bdy)
    case SchemeLetStar(bds, bdy, _)   => evalLetStar(bds, bdy)
    case SchemeLetrec(bds, bdy, _)    => evalLetrec(bds, bdy)
    case nml: SchemeNamedLet   	      => evalNamedLet(nml)
    case SchemeAnd(eps, _)            => evalAnd(eps)
    case SchemeOr(eps, _)             => evalOr(eps)
    case pai: SchemePair              => evalPair(pai)
    case spi: SchemeSplicedPair       => evalSplicedPair(spi)
		case cll: SchemeFuncall           => evalCall(cll)
    case SchemeAssert(exp, _)         => evalAssert(exp)
  }

  private def evalLambda(lam: Lam): AnalysisM[Val] =
    for { env <- getEnv } yield lattice.closure((lam, env))

	private def evalLiteralValue(exp: SchemeValue): AnalysisM[Val] = exp.value match {
		case sexp.Value.String(s)			=> storeVal(exp, lattice.string(s)) 
		case sexp.Value.Integer(n)   	=> unit(lattice.number(n))
		case sexp.Value.Real(r)      	=> unit(lattice.real(r))
		case sexp.Value.Boolean(b)   	=> unit(lattice.bool(b))
		case sexp.Value.Character(c) 	=> unit(lattice.char(c))
		case sexp.Value.Symbol(s)    	=> unit(lattice.symbol(s))
		case sexp.Value.Nil          	=> unit(lattice.nil)
	}

	private def evalVariable(nam: String): AnalysisM[Val] =
		for {
			adr <- lookupEnv(nam)
			vlu <- lookupSto(adr)
		} yield vlu

	private def evalSet(nam: String, rhs: Exp): AnalysisM[Val] = {
		for {
			rvl <- eval(rhs) 
			adr <- lookupEnv(nam)
			_ <- updateSto(adr, rvl)
		} yield lattice.void
	}

	private def evalSequence(eps: Iterable[Exp]): AnalysisM[Val] = 
		eps.foldLeftM(lattice.void)((_, exp) => eval(exp))

	private def evalIf(prd: Exp, csq: Exp, alt: Exp): AnalysisM[Val] =
		for {
			cnd <- eval(prd)
			res <- cond(cnd, eval(csq), eval(alt))
		} yield res

	private def evalLet(bds: List[(Identifier, Exp)], bdy: List[Exp]): AnalysisM[Val] = {
    val (vrs, rhs) = bds.unzip
    for {
      vls <- rhs.mapM(eval)
      ads <- vrs.mapM(allocVar)
      res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
        for {
          _ <- extendSto(ads.zip(vls))
          vlu <- evalSequence(bdy)
        } yield vlu
      }
    } yield res
  }

  private def evalLetStar(bds: List[(Identifier, Exp)], bdy: List[Exp]): AnalysisM[Val] = bds match {
    case Nil => evalSequence(bdy)
    case (vrb, rhs) :: rst =>
      for {
        vlu <- eval(rhs)
        adr <- allocVar(vrb)
        res <- withExtendedEnv(vrb.name, adr) {
          extendSto(adr, vlu).flatMap { _ =>
            evalLetStar(rst, bdy)
          }
        }
      } yield res
  }

  private def evalLetrec(bds: List[(Identifier, Exp)], bdy: List[Exp]): AnalysisM[Val] = {
    val (vrs, rhs) = bds.unzip
    for {
      ads <- vrs.mapM(allocVar)
      res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
        for {
          _ <- ads.zip(rhs).mapM_ { case (adr, rhs) => 
            eval(rhs).flatMap(vlu => extendSto(adr, vlu))
          }
          vlu <- evalSequence(bdy)
        } yield vlu 
      }
    } yield res
  }

  private def evalNamedLet(nml: SchemeNamedLet): AnalysisM[Val] = {
    val nam = nml.name
    val (prs, ags) = nml.bindings.unzip
    val lam = SchemeLambda(Some(nam.name), prs, nml.body, nml.idn)
    for {
      adr <- allocVar(nam)
      clo <- withExtendedEnv(nam.name, adr) { evalLambda(lam) } 
      _ <- extendSto(adr, clo)
      vls <- ags.mapM(eval)
      cll = SchemeFuncall(lam, ags, nml.idn)
      res <- applyFun(cll, clo, ags.zip(vls))
    } yield res
  }

  private def evalAnd(eps: List[Exp]): AnalysisM[Val] = evalAndLoop(eps, lattice.bool(true))
  private def evalAndLoop(eps: List[Exp], lst: Val): AnalysisM[Val] = eps match {
    case Nil => unit(lst)
    case exp :: rst => 
      for {
        vlu <- eval(exp)
        res <- cond(vlu, evalAndLoop(rst, vlu), unit(lattice.bool(false)))
      } yield res 
  }

  private def evalOr(eps: List[Exp]): AnalysisM[Val] = eps match {
    case Nil => unit(lattice.bool(false))
    case exp :: rst =>
      for {
        vlu <- eval(exp)
        res <- cond(vlu, unit(vlu), evalOr(rst))
      } yield res
  }

  private def evalPair(pai: SchemePair): AnalysisM[Val] =
    for {
      car <- eval(pai.car)
      cdr <- eval(pai.cdr)
      pai <- allocPai(pai, car, cdr)
    } yield pai
     
  private def evalSplicedPair(spi: SchemeSplicedPair): AnalysisM[Val] =
    for {
      spl <- eval(spi.splice)
      cdr <- eval(spi.cdr)
      res <- append(spl, cdr)
    } yield res

	// by default, asserts are ignored
	protected def evalAssert(exp: Exp): AnalysisM[Val] = unit(lattice.void)
		
	private def cond(cnd: Val, csq: AnalysisM[Val], alt: AnalysisM[Val]): AnalysisM[Val] = {
		val tru = guard(lattice.isTrue(cnd)).flatMap(_ => csq)
		val fls = guard(lattice.isFalse(cnd)).flatMap(_ => alt)
		widen(tru ++ fls)
	}

  private def evalCall(cll: SchemeFuncall): AnalysisM[Val] = 
    for {
      fun <- eval(cll.f)
      ags <- cll.args.mapM(eval)
      res <- applyFun(cll, fun, cll.args.zip(ags))
    } yield res 

  private def applyFun(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] =
    widen(applyPrimitives(cll, fun, ags) ++ applyClosures(cll, fun, ags))

  private def applyPrimitives(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] =
    for {
      prm <- pick(lattice.getPrimitives(fun)) // TODO: widen here too?
      res <- applyPrimitive(cll, primitives(prm), ags)
    } yield res

  // TODO: no widening in all primitives!
  private def applyPrimitive(cll: SchemeFuncall, prm: Prim, ags: List[(Exp, Val)]): AnalysisM[Val] =
    for {
      sto <- getSto
      ctx <- getCtx
      res <- prm.call(cll, ags, StoreAdapter(sto), InterpreterBridge(ctx)) match {
        case MayFailSuccess((vlu, sto)) => setSto(sto.sto).map { _ => vlu }
        case MayFailBoth((vlu, sto), _) => setSto(sto.sto).map { _ => vlu }
        case MayFailError(_) => mzero
      }
    } yield res

  private def applyClosures(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] = {
    val agc = ags.length
    for {
      clo <- pick(lattice.getClosures(fun)) // TODO: widen here too?
      if clo._1.check(agc) // only consider closures with compatible arity
      res <- applyClosure(cll, clo, ags)
    } yield res
  }

  private def applyClosure(cll: SchemeFuncall, clo: lattice.Closure, ags: List[(Exp, Val)]): AnalysisM[Val] = 
    for {
      ctx <- getCtx
      (lam, lex: Env @unchecked) = clo
      (fxa, vra) = ags.splitAt(lam.args.length)
      ncx = allocCtx(lam, lex, ags, cll.idn.pos, ctx) // TODO: GC before extending
      _ <- extendSto(lam.args.map(VarAddr(_, ncx)).zip(fxa.map(_._2))) // bind fixed args
      _ <- lam.varArgId match { // bind varargs
        case None => unit(())
        case Some(varArg) => allocLst(vra).flatMap(extendSto(VarAddr(varArg, ncx), _))
      }
      _ <- extendSto(EnvAddr(lam, ncx), Set(lex))
      sto <- getSto
      cmp = CallComponent(lam, ncx, sto) 
      (res, nst) <- pick(getResult(cmp))
      _ <- setSto(nst)
    } yield res

	private def storeVal(exp: Exp, vlu: Val): AnalysisM[Val] =
		for {
			adr <- allocPtr(exp)
			_	<- extendSto(adr, vlu)
		} yield lattice.pointer(adr)

  private def allocPai(pai: Exp, car: Val, cdr: Val): AnalysisM[Val] = 
    storeVal(pai, lattice.cons(car, cdr))

  protected def allocLst(els: List[(Exp, Val)]): AnalysisM[Val] = els match {
    case Nil => unit(lattice.nil)
    case (exp, vlu) :: rst =>
      for {
        rst <- allocLst(rst)
        pai <- allocPai(exp, vlu, rst)
      } yield pai
  }

  private def append(x: Val, y: Val): AnalysisM[Val] = throw new Exception("NYI -- append")

  def intraAnalysis(cmp: Component) = new IntraAnalysis(cmp) {
    def analyzeWithTimeout(timeout: Timeout.T): Unit =
      addResult(cmp, eval(cmp.exp).run(cmp.ctx, cmp.env, cmp.sto))
  }

  protected def extendV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = sto.extend(adr, V(vlu))
  protected def extendE(sto: Store[Adr, Storable], adr: Adr, evs: Set[Env]): sto.This = sto.extend(adr, E(evs))
  protected def updateV(sto: Store[Adr, Storable], adr: Adr, vlu: Val): sto.This = sto.update(adr, V(vlu))


  // UGLY CODE AHEAD -- NECESSARY TO BRIDGE WITH PRIMITIVES INTERFACE

  case class StoreAdapter[St <: Store[Adr, Storable] { type This = St }](sto: St) extends Store[Adr, Val] { outer =>
    // refine the This type
    type This = StoreAdapter[St]
    // lookup: only expect values
    def lookup(adr: Adr) = sto(adr).asInstanceOf[V].vlu match {
      case Some(V(vlu)) => Some(vlu)
      case _            => None
    }
    def extend(adr: Adr, vlu: Val) = StoreAdapter(extendV(sto, adr, vlu))
    override def update(adr: Adr, vlu: Val) = StoreAdapter(updateV(sto, adr, vlu))
    // join operations
    def empty = StoreAdapter(sto.empty)
    def join(other: This) = StoreAdapter(sto.join(other.sto))
    // delta store
    type DeltaStore = StoreAdapter[sto.DeltaStore]
    def deltaStore = StoreAdapter(sto.deltaStore)
    def integrate(delta: DeltaStore) = StoreAdapter(sto.integrate(delta.sto))
  }

  case class InterpreterBridge(ctx: Ctx) extends SchemeInterpreterBridge[Val, Adr] {
    def pointer(exp: SchemeExp): Adr = PtrAddr(exp, ctx)
    def callcc(clo: lattice.Closure, pos: Position): Value = throw new Exception("NYI")
    def currentThread: TID = throw new Exception("NYI")
  }
}
