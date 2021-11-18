package maf.modular.scheme.modflocal

import maf.core._
import maf.core.Position._
import maf.modular.scheme._
import maf.language.sexp
import maf.language.scheme._
import maf.language.scheme.primitives._

trait SchemeSemantics:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    import Monad._
    import MonadJoin._

    // shorthands
    type Val = Value
    type Adr = Address
    type Exp = SchemeExp
    type Lam = SchemeLambdaExp
    type App = SchemeFuncall
    type Env = Environment[Adr]
    type Clo = (Lam, Env)
    type Idn = Identity
    type Pos = Position

    trait AnalysisM[M[_]] extends SchemePrimM[M, Adr, Val]:
        def getEnv: M[Env]
        def withEnv[X](f: Env => Env)(blk: M[X]): M[X]
        def lookupEnv(nam: String): M[Adr] =
          for env <- getEnv yield env(nam)
        def withExtendedEnv[X](nam: String, adr: Adr)(blk: M[X]): M[X] =
          withEnv(_.extend(nam, adr))(blk)
        def withExtendedEnv[X](bds: Iterable[(String, Adr)])(blk: M[X]): M[X] =
          withEnv(_.extend(bds))(blk)
        def getCtx: M[Ctx]
        def withCtx[X](ctx: Ctx => Ctx)(blk: M[X]): M[X]
        def allocVar(idn: Identifier): M[Adr] =
          for ctx <- getCtx yield VarAddr(idn, ctx)
        def allocPtr(exp: SchemeExp): M[Adr] =
          for ctx <- getCtx yield PtrAddr(exp, ctx)
        def call(lam: Lam): M[Val]
        // Scala is too stupid to figure this out...
        implicit final private val self: AnalysisM[M] = this

    object AnalysisM:
        def apply[M[_]: AnalysisM]: AnalysisM[M] = implicitly

    type A[_]
    implicit val analysisM: AnalysisM[A]
    import analysisM._

    def eval(exp: SchemeExp): A[Val] = exp match
        case vlu: SchemeValue           => evalLiteralValue(vlu)
        case lam: SchemeLambdaExp       => evalLambda(lam)
        case SchemeVar(id)              => evalVariable(id.name)
        case SchemeBegin(eps, _)        => evalSequence(eps)
        case SchemeIf(prd, thn, alt, _) => evalIf(prd, thn, alt)
        case SchemeLet(bds, bdy, _)     => evalLet(bds, bdy)
        case SchemeLetStar(bds, bdy, _) => evalLetStar(bds, bdy)
        case SchemeLetrec(bds, bdy, _)  => evalLetrec(bds, bdy)
        case app: SchemeFuncall         => evalCall(app)
        case SchemeAssert(exp, _)       => evalAssert(exp)
        case _                          => throw new Exception(s"Unsupported Scheme expression: $exp")

    private def evalLambda(lam: Lam): A[Val] =
      for env <- getEnv yield lattice.closure((lam, env.restrictTo(lam.fv)))

    private def evalLiteralValue(exp: SchemeValue): A[Val] = exp.value match
        case sexp.Value.String(s)    => storeVal(exp, lattice.string(s))
        case sexp.Value.Integer(n)   => unit(lattice.number(n))
        case sexp.Value.Real(r)      => unit(lattice.real(r))
        case sexp.Value.Boolean(b)   => unit(lattice.bool(b))
        case sexp.Value.Character(c) => unit(lattice.char(c))
        case sexp.Value.Symbol(s)    => unit(lattice.symbol(s))
        case sexp.Value.Nil          => unit(lattice.nil)

    private def evalVariable(nam: String): A[Val] =
      for
          adr <- lookupEnv(nam)
          vlu <- lookupSto(adr)
      yield vlu

    private def evalSequence(eps: Iterable[Exp]): A[Val] =
      eps.foldLeftM(lattice.void)((_, exp) => eval(exp))

    private def evalIf(prd: Exp, csq: Exp, alt: Exp): A[Val] =
      for
          cnd <- eval(prd)
          res <- cond(cnd, eval(csq), eval(alt))
      yield res

    private def evalLet(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] =
        val (vrs, rhs) = bds.unzip
        for
            vls <- rhs.mapM(arg => eval(arg))
            ads <- vrs.mapM(allocVar)
            res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
              for
                  _ <- extendSto(ads.zip(vls))
                  vlu <- evalSequence(bdy)
              yield vlu
            }
        yield res

    private def evalLetStar(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] = bds match
        case Nil => evalSequence(bdy)
        case (vrb, rhs) :: rst =>
          for
              vlu <- eval(rhs)
              adr <- allocVar(vrb)
              res <- withExtendedEnv(vrb.name, adr) {
                extendSto(adr, vlu).flatMap { _ =>
                  evalLetStar(rst, bdy)
                }
              }
          yield res

    private def evalLetrec(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] =
        val (vrs, rhs) = bds.unzip
        for
            ads <- vrs.mapM(allocVar)
            res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
              for
                  _ <- ads.zip(rhs).mapM_ { case (adr, rhs) =>
                    eval(rhs).flatMap(vlu => extendSto(adr, vlu))
                  }
                  vlu <- evalSequence(bdy)
              yield vlu
            }
        yield res

    // by default, asserts are ignored
    protected def evalAssert(exp: Exp): A[Val] = AnalysisM[A].unit(lattice.void)

    private def cond(cnd: Val, csq: A[Val], alt: A[Val]): A[Val] =
        val tru = guard(lattice.isTrue(cnd)).flatMap(_ => csq)
        val fls = guard(lattice.isFalse(cnd)).flatMap(_ => alt)
        mjoin(tru, fls)

    private def evalCall(app: App): A[Val] =
      for
          fun <- eval(app.f)
          ags <- app.args.mapM(arg => eval(arg))
          res <- applyFun(app, fun, ags)
      yield res

    private def applyFun(app: App, fun: Val, ags: List[Val]): A[Val] =
      mjoin(applyPrimitives(app, fun, ags), applyClosures(app, fun, ags))

    private def applyPrimitives(app: App, fun: Val, ags: List[Val]): A[Val] =
      lattice.getPrimitives(fun).foldMapM { prm =>
        applyPrimitive(app, primitives(prm), ags)
      }

    protected def applyPrimitive(app: App, prm: Prim, ags: List[Val]): A[Val] =
      prm.call(app, ags)

    private def applyClosures(app: App, fun: Val, ags: List[Val]): A[Val] =
        val agc = ags.length
        lattice.getClosures(fun).foldMapM { (lam, lex) =>
          for
              _ <- guard(lam.check(agc))
              fvs <- lex.addrs.mapM(adr => lookupSto(adr).map((adr, _)))
              res <- applyClosure(app, lam, ags, fvs)
          yield res
        }

    protected def applyClosure(app: App, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[Val] =
      withCtx(newContext(app, lam, ags, _)) {
        argBindings(app, lam, ags, fvs) >>= { bds =>
            val envBds = bds.map((nam, adr, _) => (nam, adr))
            val stoBds = bds.map((_, adr, vlu) => (adr, vlu))
            withEnv(_ => Environment(envBds)) {
              extendSto(stoBds) >>> call(lam)
            }
        }
      }

    private def argBindings(app: App, lam: Lam, ags: List[Val], fvs: Iterable[(Adr, Val)]): A[List[(String, Adr, Val)]] =
      for
          // fixed args
          fxa <- lam.args.zip(ags).mapM((idf, vlu) => allocVar(idf).map((idf.name, _, vlu)))
          // vararg (optional)
          vra <- lam.varArgId match
              case None => unit(Nil)
              case Some(varArg) =>
                val len = lam.args.length
                val (_, vag) = ags.splitAt(len)
                val (_, vex) = app.args.splitAt(len)
                for
                    lst <- allocLst(vex.zip(vag))
                    adr <- allocVar(varArg)
                yield List((varArg.name, adr, lst))
          // free variables
          frv <- fvs.mapM((adr, vlu) =>
            adr match {
              case VarAddr(idf, _) => allocVar(idf).map((idf.name, _, vlu))
              case PrmAddr(nam)    => unit((nam, adr, vlu))
            }
          )
      yield fxa ++ vra ++ frv

    private def storeVal(exp: Exp, vlu: Val): A[Val] =
      for
          adr <- allocPtr(exp)
          _ <- extendSto(adr, vlu)
      yield lattice.pointer(adr)

    private def allocPai(pai: Exp, car: Val, cdr: Val): A[Val] =
      storeVal(pai, lattice.cons(car, cdr))

    protected def allocLst(els: List[(Exp, Val)]): A[Val] = els match
        case Nil => unit(lattice.nil)
        case (exp, vlu) :: rst =>
          for
              rst <- allocLst(rst)
              pai <- allocPai(exp, vlu, rst)
          yield pai
