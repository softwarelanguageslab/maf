package maf.analysis

import maf.util.*
import maf.analysis.store.*
import maf.syntax.sexp.*
import maf.syntax.scheme.*
import cats.syntax.all.*
import maf.values.scheme.SchemeLattice
import maf.syntax.*
import primitives.*
import maf.values.typeclasses.Galois.*
import maf.interpreter.*
import maf.interpreter.ConcreteSchemeValue.given
import maf.values.scheme.given
import maf.values.typeclasses.Galois.inject

type Var = Identifier[SchemeExp]
type Adr = Address
type Env = Environment[Adr]
type Exp = SchemeExp
type Lam = SchemeLambdaExp
type App = SchemeFuncall
type Clo = (Lam, Env)
type Idn = Identity
type Prim = SchemePrimitive[Value, Address]

trait AnalysisM[M[_], Val] extends SchemePrimM[M, Val, Adr]:
    type Ctx
    type Env = Environment[Adr]
    def getEnv: M[Env]
    def withEnv[X](f: Env => Env)(blk: M[X]): M[X]
    def withEnvM[X](f: Env => M[Env])(blk: M[X]): M[X] =
        for
            oldEnv <- getEnv
            newEnv <- f(oldEnv)
            res <- withEnv(_ => newEnv) { blk }
        yield res
    def lookupEnv(id: Var): M[Adr] =
        getEnv map (_.lookup(id.name).getOrElse(
          throw new Exception(s"undefined variable ${id.name}@${id.idn}")
        ))
    def withExtendedEnv[X](nam: String, adr: Adr)(blk: M[X]): M[X] =
        withEnv(_.extend(nam, adr))(blk)
    def withExtendedEnv[X](bds: Iterable[(String, Adr)])(blk: M[X]): M[X] =
        withEnv(_.extend(bds))(blk)
    def getCtx: M[Ctx]
    def withCtx[X](ctx: Ctx => Ctx)(blk: M[X]): M[X]
    def allocVar(idn: Identifier[SchemeExp]): M[Adr] =
        for ctx <- getCtx yield VarAddr(idn, ctx)
    def allocPtr(exp: SchemeExp): M[Adr] =
        for ctx <- getCtx yield PtrAddr(exp, ctx)
    def call(lam: Lam): M[Val]
    def nontail[A](blk: => M[A]): M[A] = blk
    // Scala is too stupid to figure this out...
    implicit final private val self: AnalysisM[M, Val] = this

object AnalysisM:
    def apply[M[_], Val, Ctx](
        using
        anl: AnalysisM[M, Val]
      ): AnalysisM[M, Val] = anl

trait SchemeContextSensitivityM[M[_], Val] extends AnalysisM[M, Val]:
    def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx

trait SchemeSemanticsM[M[_], Val: SchemeDomain] extends AnalysisM[M, Val] with SchemeContextSensitivityM[M, Val]:
    val primitives = SchemeDomain[Val].primitives
    given lattice: SchemeLattice[Val] = SchemeDomain[Val].lattice

    def eval(e: SchemeExp): M[Val]

/** Trait that introduces utility functions when mixed in */
trait Semantics:
    def withM[A[_], Val, Ctx, X](
        using sem: SchemeSemanticsM[A, Val]
      )(f: SchemeSemanticsM[A, Val] => A[X]
      ): A[X] =
        f(sem)

object SchemeSemanticsM:
    def apply[A[_], Adr, Val](using sem: SchemeSemanticsM[A, Val]) = sem

object SchemeSemantics extends Semantics:
    // shorthands
    type Val = Value
    type M[A[_]] = SchemeSemanticsM[A, Value]

    def eval[A[_]: M](exp: SchemeExp): A[Val] = exp match
        case vlu: SchemeValue           => evalLiteralValue(vlu)
        case lam: SchemeLambdaExp       => evalLambda(lam)
        case SchemeVar(id)              => evalVariable(id)
        case SchemeBegin(eps, _)        => evalSequence(eps)
        case SchemeIf(prd, thn, alt, _) => evalIf(prd, thn, alt)
        case SchemeLet(bds, bdy, _)     => evalLet(bds, bdy)
        case SchemeLetStar(bds, bdy, _) => evalLetStar(bds, bdy)
        case SchemeLetrec(bds, bdy, _)  => evalLetrec(bds, bdy)
        case app: SchemeFuncall         => evalCall(app)
        case SchemeAssert(exp, _)       => evalAssert(exp)
        case _                          => throw new Exception(s"Unsupported Scheme expression: $exp")

    def evalAll[A[_]](lst: List[SchemeExp])(using m: M[A]): A[List[Val]] = lst match
        case Nil         => Nil.pure
        case last :: Nil => m.eval(last).map(_ :: Nil)
        case next :: rest =>
            for
                v <- m.eval(next)
                vs <- evalAll(rest)
            yield v :: vs

    private def evalLambda[A[_]: M](lam: Lam): A[Val] =
        val m = summon[M[A]]
        m.getEnv map (env => m.lattice.injectClosure(lam, env.restrictTo(lam.fv)))

    private def evalLiteralValue[A[_]: M](exp: SchemeValue): A[Val] = withM { m =>
        import m.given

        exp.value match
            case sexp.Value.String(s)    => storeVal(exp, inject[String, Val](s))
            case sexp.Value.Integer(n)   => inject(n).pure
            case sexp.Value.Real(r)      => inject(r).pure
            case sexp.Value.Boolean(b)   => inject(b).pure
            case sexp.Value.Character(c) => inject(c).pure
            case sexp.Value.Symbol(s)    => inject(s).pure
            case sexp.Value.Nil          => inject[SimpleSchemeValue, Val](SchemeNil).pure
    }

    private def evalVariable[A[_]: M](vrb: Var): A[Val] = withM { m =>
        for
            adr <- m.lookupEnv(vrb)
            vlu <- m.lookupSto(adr)
        yield vlu
    }

    private def evalSequence[A[_]: M](eps: Iterable[Exp]): A[Val] = withM { m =>
        import m.{given, *}
        eps match
            case Nil          => inject[SimpleSchemeValue, Val](SchemeVoid).pure
            case last :: Nil  => m.eval(last)
            case next :: rest => m.eval(next) >> evalSequence(rest)
    }

    private def evalIf[A[_]: M](prd: Exp, csq: Exp, alt: Exp): A[Val] = withM { m =>
        for
            cnd <- m.eval(prd)
            res <- cond(cnd, m.eval(csq), m.eval(alt))
        yield res
    }

    private def evalLet[A[_]: M](
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      ): A[Val] = withM { m =>
        val (vrs, rhs) = bds.unzip
        for
            vls <- evalAll(rhs)
            ads <- vrs.traverse(m.allocVar)
            res <- m.withExtendedEnv(vrs.map(_.name).zip(ads)) {
                m.extendSto(ads.zip(vls)) >> evalSequence(bdy)
            }
        yield res
    }

    private def evalLetStar[A[_]: M](
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      ): A[Val] = withM { m =>
        bds match
            case Nil => evalSequence(bdy)
            case (vrb, rhs) :: rst =>
                for
                    vlu <- m.eval(rhs)
                    adr <- m.allocVar(vrb)
                    res <- m.withExtendedEnv(vrb.name, adr) {
                        m.extendSto(adr, vlu) >> evalLetStar(rst, bdy)
                    }
                yield res
    }

    private def evalLetrec[A[_]](
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      )(using m: M[A]
      ): A[Val] =
        val (vrs, rhs) = bds.unzip
        for
            ads <- vrs.traverse(m.allocVar)
            res <- m.withExtendedEnv(vrs.map(_.name).zip(ads)) {
                for
                    _ <- ads.zip(rhs).traverse_ { case (adr, rhs) =>
                        m.eval(rhs) >>= (vlu => m.extendSto(adr, vlu))
                    }
                    vlu <- evalSequence(bdy)
                yield vlu
            }
        yield res

    // by default, asserts are ignored
    private def evalAssert[A[_]: M](exp: Exp): A[Val] = withM { m =>
        import m.given
        inject[SimpleSchemeValue, Val](SchemeVoid).pure
    }

    private def cond[A[_]: M](cnd: Val, csq: A[Val], alt: A[Val]): A[Val] =
        withM { m =>
            import m.{given, *}

            m.cond(cnd) /* then */ { csq } /* else */ { alt }
        }

    private def evalCall[A[_]](app: App)(using m: M[A]): A[Val] =
        import m.{given, *}
        for
            fun <- m.eval(app.f)
            ags <- evalAll(app.args)
            res <- applyFun(app, fun, ags)
        yield res

    private def applyFun[A[_]](app: App, fun: Val, ags: List[Val])(using m: M[A]): A[Val] =
        import m.{given, *}
        m.mfoldMap(m.lattice.split(fun)) {
            case m.lattice.primitive(prm, _) =>
                applyPrimitive(app, m.primitives.allPrimitives(prm), ags)
            case m.lattice.closures((lam, lex), _) =>
                applyClosure(app, lam, lex, ags)
            case v => raiseError(TypeError("value cannot applied", v))
        }

    private def applyPrimitive[A[_]: M](
        app: App,
        prm: Prim,
        ags: List[Val]
      ): A[Val] =
        prm.call(app, ags)

    private def applyClosure[A[_]](
        app: App,
        lam: Lam,
        lex: Env,
        ags: List[Val]
      )(using m: M[A]
      ): A[Val] =
        import m.{given, *}
        val agc = ags.length
        for
            _ <- guard(lam.check(agc)) {
                raiseError(ArityError(lam.idn, lam.args.size, agc, app.idn))
            }
            fvs <- lex.addrs.toList.traverse(adr => lookupSto(adr).map((adr, _)))
            result <- withCtx(newContext(app, lam, ags, _)) {
                argBindings(app, lam, ags, fvs) >>= { bds =>
                    val envBds = bds.map((nam, adr, _) => (nam, adr))
                    val stoBds = bds.map((_, adr, vlu) => (adr, vlu))
                    withEnv(_ => Environment(envBds)) {
                        extendSto(stoBds) >> call(lam)
                    }
                }
            }
        yield result

    private def argBindings[A[_]](
        app: App,
        lam: Lam,
        ags: List[Val],
        fvs: Iterable[(Adr, Val)]
      )(using m: M[A]
      ): A[List[(String, Adr, Val)]] =
        import m.*
        for
            // fixed args
            fxa <- lam.args
                .zip(ags)
                .traverse((idf, vlu) => allocVar(idf).map((idf.name, _, vlu)))
            // vararg (optional)
            vra <- lam.varArgId match
                case None => Nil.pure
                case Some(varArg) =>
                    val len = lam.args.length
                    val (_, vag) = ags.splitAt(len)
                    val (_, vex) = app.args.splitAt(len)
                    for
                        lst <- allocLst(vex.zip(vag))
                        adr <- allocVar(varArg)
                    yield List((varArg.name, adr, lst))
            // free variables
            frv <- fvs.toList.traverse { (adr, vlu) =>
                adr match
                    case VarAddr(idf, _) => allocVar(idf).map((idf.name, _, vlu))
                    case PrmAddr(nam)    => (nam, adr, vlu).pure
            }
        yield fxa ++ vra ++ frv

    private def storeVal[A[_]](exp: Exp, vlu: Val)(using m: M[A]): A[Val] =
        import m.{given, *}
        for
            adr <- allocPtr(exp)
            _ <- extendSto(adr, vlu)
        yield lattice.pointer(adr)

    private def allocPai[A[_]](pai: Exp, car: Val, cdr: Val)(using m: M[A]): A[Val] =
        storeVal(pai, m.lattice.cons(car, cdr))

    private def allocLst[A[_]](els: List[(Exp, Val)])(using m: M[A]): A[Val] =
        import m.{given, *}
        els match
            case Nil => inject[SimpleSchemeValue, Val](SchemeNil).pure
            case (exp, vlu) :: rst =>
                for
                    rst <- allocLst(rst)
                    pai <- allocPai(exp, vlu, rst)
                yield pai
