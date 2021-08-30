package maf.modular.scheme.modflocal

import maf.core._
import maf.core.Position._
import maf.modular.scheme._
import maf.language.sexp
import maf.language.scheme._
import maf.language.scheme.primitives._

trait SchemeSemantics {
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import Monad._
  import MonadJoin._

  // shorthands
  type Val = Value
  type Adr = Address
  type Exp = SchemeExp
  type Lam = SchemeLambdaExp
  type Cll = SchemeFuncall
  type Clo = (Lam, Environment[Adr])
  type Env = NestedEnv[Adr, EnvAddr]
  type Idn = Identity
  type Pos = Position

  case class EnvAddr(lam: Lam, ctx: Ctx) extends Address {
    def idn = lam.idn
    def printable = true
    override def toString = s"EnvAddr(${lam.lambdaName}, $ctx)"
  }

  trait AnalysisM[M[_]] extends SchemePrimM[M, Adr, Val] {
    def getEnv: M[Env]
    def lookupEnv[X: Lattice](nam: String)(f: Adr => M[X]): M[X] = {
      def find(cur: Env): M[X] =
        cur.lookup(nam) match {
          case Some(adr) => f(adr)
          case None =>
            for {
              evs <- lookupEnvSto(cur.rst.get)
              res <- evs.foldMapM(find)
            } yield res
        }
      flatMap(getEnv)(find)
    }
    def withExtendedEnv[X](nam: String, adr: Adr)(blk: M[X]): M[X]
    def withExtendedEnv[X](bds: Iterable[(String, Adr)])(blk: M[X]): M[X] =
      bds.foldRight(blk) { case ((nam, adr), rst) => withExtendedEnv(nam, adr)(rst) }
    def getCtx: M[Ctx]
    def allocVar(idn: Identifier): M[Adr] =
      for { ctx <- getCtx } yield VarAddr(idn, ctx)
    def allocPtr(exp: SchemeExp): M[Adr] =
      for { ctx <- getCtx } yield PtrAddr(exp, ctx)
    def allocCtx(cll: Cll, clo: Clo, ags: List[Val]): M[Ctx] =
      for { ctx <- getCtx } yield newContext(cll, clo, ags, ctx)
    def writePar(par: Identifier, ctx: Ctx, arg: Val): M[Unit] =
      extendSto(VarAddr(par, ctx), arg)
    def writePar(prs: Iterable[(Identifier, Val)], ctx: Ctx): M[Unit] =
      prs.mapM_[M, Unit] { case (par, vlu) => writePar(par, ctx, vlu) }
    def writeEnv(lam: Lam, ctx: Ctx, env: Env): M[Unit] =
      extendEnvSto(EnvAddr(lam, ctx), Set(env))
    def extendEnvSto(adr: EnvAddr, evs: Set[Env]): M[Unit]
    def lookupEnvSto(adr: EnvAddr): M[Set[Env]]
    def call(lam: Lam, ctx: Ctx): M[Val]
    // Scala is too stupid to figure this out...
    implicit final private val self: AnalysisM[M] = this
  }

  object AnalysisM {
    def apply[M[_]: AnalysisM]: AnalysisM[M] = implicitly
  }

  type A[_]
  implicit val analysisM: AnalysisM[A]
  import analysisM._

  def eval(exp: SchemeExp): A[Val] = exp match {
    case vlu: SchemeValue           => evalLiteralValue(vlu)
    case lam: SchemeLambdaExp       => evalLambda(lam)
    case SchemeVar(id)              => evalVariable(id.name)
    case SchemeSet(id, rhs, _)      => evalSet(id.name, rhs)
    case SchemeBegin(eps, _)        => evalSequence(eps)
    case SchemeIf(prd, thn, alt, _) => evalIf(prd, thn, alt)
    case SchemeLet(bds, bdy, _)     => evalLet(bds, bdy)
    case SchemeLetStar(bds, bdy, _) => evalLetStar(bds, bdy)
    case SchemeLetrec(bds, bdy, _)  => evalLetrec(bds, bdy)
    case nml: SchemeNamedLet        => evalNamedLet(nml)
    case SchemeAnd(eps, _)          => evalAnd(eps)
    case SchemeOr(eps, _)           => evalOr(eps)
    case pai: SchemePair            => evalPair(pai)
    case spi: SchemeSplicedPair     => evalSplicedPair(spi)
    case cll: SchemeFuncall         => evalCall(cll)
    case SchemeAssert(exp, _)       => evalAssert(exp)
    case _                          => throw new Exception(s"Unsupported Scheme expression: $exp")
  }

  private def evalLambda(lam: Lam): A[Val] =
    for { env <- getEnv } yield lattice.closure((lam, env))

  private def evalLiteralValue(exp: SchemeValue): A[Val] = exp.value match {
    case sexp.Value.String(s)    => storeVal(exp, lattice.string(s))
    case sexp.Value.Integer(n)   => unit(lattice.number(n))
    case sexp.Value.Real(r)      => unit(lattice.real(r))
    case sexp.Value.Boolean(b)   => unit(lattice.bool(b))
    case sexp.Value.Character(c) => unit(lattice.char(c))
    case sexp.Value.Symbol(s)    => unit(lattice.symbol(s))
    case sexp.Value.Nil          => unit(lattice.nil)
  }

  private def evalVariable(nam: String): A[Val] =
    lookupEnv(nam)(adr => lookupSto(adr))

  private def evalSet(nam: String, rhs: Exp): A[Val] = {
    for {
      rvl <- eval(rhs)
      res <- lookupEnv(nam)(adr => updateSto(adr, rvl))
    } yield lattice.void
  }

  private def evalSequence(eps: Iterable[Exp]): A[Val] =
    eps.foldLeftM(lattice.void)((_, exp) => eval(exp))

  private def evalIf(prd: Exp, csq: Exp, alt: Exp): A[Val] =
    for {
      cnd <- eval(prd)
      res <- cond(cnd, eval(csq), eval(alt))
    } yield res

  private def evalLet(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] = {
    val (vrs, rhs) = bds.unzip
    for {
      vls <- rhs.mapM(arg => eval(arg))
      ads <- vrs.mapM(allocVar)
      res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
        for {
          _ <- extendSto(ads.zip(vls))
          vlu <- evalSequence(bdy)
        } yield vlu
      }
    } yield res
  }

  private def evalLetStar(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] = bds match {
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

  private def evalLetrec(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] = {
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

  private def evalNamedLet(nml: SchemeNamedLet): A[Val] = {
    val nam = nml.name
    val (prs, ags) = nml.bindings.unzip
    val lam = SchemeLambda(Some(nam.name), prs, nml.body, nml.idn)
    for {
      adr <- allocVar(nam)
      clo <- withExtendedEnv(nam.name, adr) { evalLambda(lam) }
      _ <- extendSto(adr, clo)
      vls <- ags.mapM(arg => eval(arg))
      cll = SchemeFuncall(lam, ags, nml.idn)
      res <- applyFun(cll, clo, vls)
    } yield res
  }

  private def evalAnd(eps: List[Exp]): A[Val] = evalAndLoop(eps, lattice.bool(true))
  private def evalAndLoop(eps: List[Exp], lst: Val): A[Val] = eps match {
    case Nil => AnalysisM[A].unit(lst)
    case exp :: rst =>
      for {
        vlu <- eval(exp)
        res <- cond(vlu, evalAndLoop(rst, vlu), AnalysisM[A].unit(lattice.bool(false)))
      } yield res
  }

  private def evalOr(eps: List[Exp]): A[Val] = eps match {
    case Nil => unit(lattice.bool(false))
    case exp :: rst =>
      for {
        vlu <- eval(exp)
        res <- cond(vlu, AnalysisM[A].unit(vlu), evalOr(rst))
      } yield res
  }

  private def evalPair(pai: SchemePair): A[Val] =
    for {
      car <- eval(pai.car)
      cdr <- eval(pai.cdr)
      pai <- allocPai(pai, car, cdr)
    } yield pai

  private def evalSplicedPair(spi: SchemeSplicedPair): A[Val] =
    for {
      spl <- eval(spi.splice)
      cdr <- eval(spi.cdr)
      res <- append(spl, cdr)
    } yield res

  // by default, asserts are ignored
  protected def evalAssert(exp: Exp): A[Val] = AnalysisM[A].unit(lattice.void)

  private def cond(cnd: Val, csq: A[Val], alt: A[Val]): A[Val] = {
    val tru = guard(lattice.isTrue(cnd)).flatMap(_ => csq)
    val fls = guard(lattice.isFalse(cnd)).flatMap(_ => alt)
    mjoin(tru, fls)
  }

  private def evalCall(cll: SchemeFuncall): A[Val] =
    for {
      fun <- eval(cll.f)
      ags <- cll.args.mapM(arg => eval(arg))
      res <- applyFun(cll, fun, ags)
    } yield res

  private def applyFun(cll: Cll, fun: Val, ags: List[Val]): A[Val] =
    mjoin(applyPrimitives(cll, fun, ags), applyClosures(cll, fun, ags))

  private def applyPrimitives(cll: Cll, fun: Val, ags: List[Val]): A[Val] =
    lattice.getPrimitives(fun).foldMapM { prm =>
      applyPrimitive(cll, primitives(prm), ags)
    }

  private def applyPrimitive(cll: Cll, prm: Prim, ags: List[Val]): A[Val] =
    prm.call(cll, ags)

  private def applyClosures(cll: Cll, fun: Val, ags: List[Val]): A[Val] = {
    val agc = ags.length
    lattice.getClosures(fun).foldMapM { clo =>
      guard(clo._1.check(agc)).flatMap { _ =>
        applyClosure(cll, clo, ags)
      }
    }
  }

  protected def applyClosure(cll: Cll, clo: Clo, ags: List[Val]): A[Val] =
    // TODO: GC here
    for {
      ctx <- allocCtx(cll, clo, ags)
      _ <- bindArgs(cll, clo, ags, ctx)
      res <- call(clo._1, ctx)
    } yield res

  private def bindArgs(cll: Cll, clo: Clo, ags: List[Val], ctx: Ctx): A[Unit] = {
    val (lam, lex: Env @unchecked) = clo
    for {
      _ <- writePar(lam.args.zip(ags), ctx) // bind fixed args
      _ <- lam.varArgId match { // bind varargs
        case None => unit(())
        case Some(varArg) =>
          val len = lam.args.length
          val (_, vag) = ags.splitAt(len)
          val (_, vex) = cll.args.splitAt(len)
          allocLst(vex.zip(vag)).flatMap(writePar(varArg, ctx, _))
      }
      _ <- writeEnv(lam, ctx, lex) //bind env
    } yield ()
  }

  private def storeVal(exp: Exp, vlu: Val): A[Val] =
    for {
      adr <- allocPtr(exp)
      _ <- extendSto(adr, vlu)
    } yield lattice.pointer(adr)

  private def allocPai(pai: Exp, car: Val, cdr: Val): A[Val] =
    storeVal(pai, lattice.cons(car, cdr))

  protected def allocLst(els: List[(Exp, Val)]): A[Val] = els match {
    case Nil => unit(lattice.nil)
    case (exp, vlu) :: rst =>
      for {
        rst <- allocLst(rst)
        pai <- allocPai(exp, vlu, rst)
      } yield pai
  }

  private def append(x: Val, y: Val): A[Val] =
    throw new Exception("NYI -- append")
}
