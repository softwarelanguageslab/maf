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

abstract class SchemeModFLocal(prg: SchemeExp) extends ModAnalysis[SchemeExp](prg) with SchemeDomain {
  this: SchemeModFLocalSensitivity =>

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

  // AnalysisM monad
  import Monad._
  import MonadJoin._
  
  case class AnalysisM[X](run: (Ctx, Env, Sto) => Set[(X, Sto)])

  implicit case object AnalysisMInstances extends Monad[AnalysisM] with MonadError[AnalysisM, Error] with MonadJoin[AnalysisM] with SchemePrimM[AnalysisM, Adr, Val] {
    // MONAD
    def unit[X](x: X) = 
      AnalysisM((_, _, sto) => Set((x, sto)))
    def map[X, Y](m: AnalysisM[X])(f: X => Y) =
      AnalysisM((ctx, env, sto) => m.run(ctx, env, sto).map(res => (f(res._1), res._2)))
    def flatMap[X, Y](m: AnalysisM[X])(f: X => AnalysisM[Y]) =
      AnalysisM((ctx, env, sto) => m.run(ctx, env, sto).flatMap(res => f(res._1).run(ctx, env, res._2)))
    // MONADJOIN
    def mbottom[X] = 
      AnalysisM((_,_,_) => Set.empty)
    def mjoin[X: Lattice](x: AnalysisM[X], y: AnalysisM[X]) = 
      AnalysisM((ctx, env, sto) => x.run(ctx, env, sto) ++ y.run(ctx, env, sto))
    // MONADERROR
    def fail[X](err: Error) = 
      mbottom // we are not interested in errors here (at least, not yet ...)
    // SCHEMEPRIMM
    def addrEq: AnalysisM[MaybeEq[Adr]] = 
      AnalysisM((_, _, sto) => Set((sto.addrEq, sto)))
    def getCtx: AnalysisM[Ctx] =
      AnalysisM((ctx, _, sto) => Set((ctx, sto)))
    def allocPtr(exp: SchemeExp) = 
		  map(getCtx)(PtrAddr(exp, _))
	  def allocVar(vrb: Identifier) =
		  map(getCtx)(VarAddr(vrb, _))
    def extendSto(adr: Adr, vlu: Val): AnalysisM[Unit] = 
      AnalysisM((_, env, sto) => Set(((), extendV(sto, adr, vlu))))
    def extendSto(bds: Iterable[(Adr, Val)]): AnalysisM[Unit] = 
      bds.mapM_ { case (adr, vlu) => extendSto(adr, vlu) }
  	def updateSto(adr: Adr, vlu: Val) =  //TODO: GC after an update?
      AnalysisM((_, env, sto) => Set(((), updateV(sto, adr, vlu))))
    def lookupSto(adr: Adr) = 
      AnalysisM((_, _, sto) => Set((sto(adr).asInstanceOf[V].vlu, sto)))
    def getSto: AnalysisM[Sto] =
      AnalysisM((_, env, sto) => Set((sto, sto))) 
    def setSto(sto: Sto): AnalysisM[Unit] =
      AnalysisM((_, env, _) => Set(((), sto)))
    // ENV STUFF
    def getEnv: AnalysisM[Env] = AnalysisM((_, env, sto) => Set((env, sto)))
      AnalysisM((ctx, env, sto) => Set((env, sto)))
    def withEnv[X](f: Env => Env)(blk: AnalysisM[X]): AnalysisM[X] =
      AnalysisM((ctx, env, sto) => blk.run(ctx, f(env), sto))
    def withEnv[X](env: Env)(blk: AnalysisM[X]): AnalysisM[X] =
      withEnv(_ => env)(blk)
    def withExtendedEnv[X](nam: String, adr: VarAddr[Ctx])(blk: AnalysisM[X]): AnalysisM[X] =
		  withEnv(_.extend(nam, adr))(blk)
    def withExtendedEnv[X](bds: Iterable[(String,VarAddr[Ctx])])(blk: AnalysisM[X]): AnalysisM[X] =
		  withEnv(_.extend(bds))(blk)
    def lookupEnv[X: Lattice](nam: String)(f: Adr => AnalysisM[X]): AnalysisM[X] =
      for {
        env <- getEnv
        res <- env.lookup(nam) match {
          case Some(adr) => f(adr) 
          case None => 
            for {
              evs <- lookupEnvSto(env.rst.get)
              res <- evs.foldMapM { nxt =>
                withEnv(nxt)(lookupEnv(nam)(f))
              }
            } yield res
        }
      } yield res
	  def extendEnvSto(adr: EnvAddr, evs: Set[Env]): AnalysisM[Unit] = 
      AnalysisM((_, _, sto) => Set(((), extendE(sto, adr, evs))))
    def lookupEnvSto(adr: EnvAddr): AnalysisM[Set[Env]] =
      AnalysisM((_, _, sto) => Set((sto(adr).asInstanceOf[E].evs, sto)))
    def allocCtx(fex: Exp, clo: lattice.Closure, ags: List[(Exp, Val)]): AnalysisM[Ctx] =
      for { ctx <- getCtx } yield newContext(fex, clo, ags, ctx)
    def call(lam: Lam, ctx: Ctx): AnalysisM[Val] = 
      AnalysisM((_, _, sto) => getResult(CallComponent(lam, ctx, sto)))
  } 

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

  def initialComponent: Component = MainComponent
  def expr(cmp: Component): Exp = cmp.exp

  // results
  var results: Map[Component, Set[(Val, Sto)]] = Map.empty
  def addResult(cmp: Component, res: Set[(Val, Sto)]) =
    results += cmp -> (getResult(cmp) ++ res)
  def getResult(cmp: Component): Set[(Val, Sto)] =
    results.getOrElse(cmp, Set.empty)

  import AnalysisMInstances._

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
    case _ => throw new Exception(s"Unsupported Scheme expression: $exp")
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
		lookupEnv(nam)(adr => lookupSto(adr))

	private def evalSet(nam: String, rhs: Exp): AnalysisM[Val] = {
		for {
			rvl <- eval(rhs) 
			_ <- lookupEnv(nam)(adr => updateSto(adr, rvl))
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
		mjoin(tru, fls)
	}

  private def evalCall(cll: SchemeFuncall): AnalysisM[Val] = 
    for {
      fun <- eval(cll.f)
      ags <- cll.args.mapM(eval)
      res <- applyFun(cll, fun, cll.args.zip(ags))
    } yield res 

  private def applyFun(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] =
    mjoin(applyPrimitives(cll, fun, ags), applyClosures(cll, fun, ags))

  private def applyPrimitives(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] =
    lattice.getPrimitives(fun).foldMapM { prm =>
      applyPrimitive(cll, primitives(prm), ags)
    }

  private def applyPrimitive(cll: SchemeFuncall, prm: Prim, ags: List[(Exp, Val)]): AnalysisM[Val] =
    prm.call(cll, ags)

  private def applyClosures(cll: SchemeFuncall, fun: Val, ags: List[(Exp, Val)]): AnalysisM[Val] = {
    val agc = ags.length
    lattice.getClosures(fun).foldMapM { clo =>
      guard(clo._1.check(agc)).flatMap { _ =>
        applyClosure(cll, clo, ags)
      }
    }
  }

  private def applyClosure(cll: SchemeFuncall, clo: lattice.Closure, ags: List[(Exp, Val)]): AnalysisM[Val] = 
    // TODO: GC here
    for {
      ctx <- allocCtx(cll, clo, ags)
      _ <- bindArgs(clo, ags, ctx)
      res <- call(clo._1, ctx)
    } yield res

  private def bindArgs(clo: lattice.Closure, ags: List[(Exp,Val)], ctx: Ctx): AnalysisM[Unit] = {
    val (lam, lex: Env @unchecked) = clo
    val (fxa, vra) = ags.splitAt(lam.args.length)
    for {
      _ <- extendSto(lam.args.map(VarAddr(_, ctx)).zip(fxa.map(_._2))) // bind fixed args
      _ <- lam.varArgId match { // bind varargs
        case None => unit(())
        case Some(varArg) => allocLst(vra).flatMap(extendSto(VarAddr(varArg, ctx), _))
      }
      _ <- extendEnvSto(EnvAddr(lam, ctx), Set(lex))
    } yield ()
  }

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
}
