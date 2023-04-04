package maf.analysis

import maf.util.*
import maf.analysis.store.*
import maf.syntax.sexp.*
import maf.syntax.scheme.*
import cats.syntax.all.*
import cats.Monad
import maf.values.scheme.*
import maf.syntax.*
import primitives.*
import maf.values.typeclasses.Galois.*
import maf.interpreter.*
import maf.interpreter.ConcreteSchemeValue.given
import maf.values.scheme.given
import maf.values.typeclasses.Galois.inject

type Var = Identifier[SchemeExp]
type Exp = SchemeExp
type Adr = Address

trait EnvironmentM[M[_], Val] extends Monad[M]:
    private given self: Monad[M] = this

    type Env = Environment[ValAddress[Val]]

    def getEnv: M[Env]
    def withEnv[X](f: Env => Env)(blk: M[X]): M[X]
    def withEnvM[X](f: Env => M[Env])(blk: M[X]): M[X] =
        for
            oldEnv <- getEnv
            newEnv <- f(oldEnv)
            res <- withEnv(_ => newEnv) { blk }
        yield res

    def lookupEnv(id: Var): M[ValAddress[Val]] =
        getEnv map (_.lookup(id.name).getOrElse(
          throw new Exception(s"undefined variable ${id.name}@${id.idn}")
        ))
    def withExtendedEnv[X](nam: String, adr: ValAddress[Val])(blk: M[X]): M[X] =
        withEnv(_.extend(nam, adr))(blk)
    def withExtendedEnv[X](bds: Iterable[(String, ValAddress[Val])])(blk: M[X]): M[X] =
        withEnv(_.extend(bds))(blk)

trait AnalysisM[M[_], Val, Vec, Pai] extends SchemePrimM[M, Val, Vec, Pai]:
    type Lam = SchemeLambdaExp
    type Env = Environment[ValAddress[Val]]
    type Ctx

    def getCtx: M[Ctx]
    def withCtx[X](ctx: Ctx => Ctx)(blk: M[X]): M[X]
    def call(lam: Lam): M[Val]

    // Scala is too stupid to figure this out...
    given self: AnalysisM[M, Val, Vec, Pai] = this

object AnalysisM:
    def apply[M[_], Val, Vec, Pai](
        using
        anl: AnalysisM[M, Val, Vec, Pai]
      ): AnalysisM[M, Val, Vec, Pai] = anl

trait SchemeContextSensitivityM[M[_], Val, Vec, Pai] extends AnalysisM[M, Val, Vec, Pai]:
    def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx

trait SchemeValues[V, Vec, Pai]:
    type Prim = SchemePrimitive[V, Vec, Pai, Adr]
    type Val = V

    given domain: SchemeDomain[V, Vec, Pai]
    given primitives: SchemeLatticePrimitives[V, Vec, Pai]
    given lattice: SchemeLattice[V, Vec, Pai] = domain.schemeLattice

trait SchemeSemanticsM[M[_], V, Vec, Pai] extends AnalysisM[M, V, Vec, Pai] with SchemeContextSensitivityM[M, V, Vec, Pai] with EnvironmentM[M, V]:
    def eval(e: SchemeExp): M[V]

object SchemeSemanticsM:
    def apply[A[_], Val, Vec, Pai](using sem: SchemeSemanticsM[A, Val, Vec, Pai]) = sem

final class SchemeSemantics[A[_], V, Vec, Pai](using dom: SchemeValues[V, Vec, Pai], m: SchemeSemanticsM[A, V, Vec, Pai]):
    // shorthands
    type M[A[_], V] = SchemeSemanticsM[A, V, Vec, Pai]
    type App = SchemeFuncall
    type Lam = SchemeLambdaExp
    type Val = V
    type Prim = SchemePrimitive[V, Vec, Pai, Adr]
    type Env = Environment[Address]

    import dom.given
    import SchemeLattice.given

    def eval(exp: SchemeExp): A[V] = exp match
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

    def evalAll(lst: List[SchemeExp]): A[List[V]] = lst match
        case Nil         => Nil.pure
        case last :: Nil => m.eval(last).map(_ :: Nil)
        case next :: rest =>
            for
                v <- m.eval(next)
                vs <- evalAll(rest)
            yield v :: vs

    private def evalLambda(lam: Lam): A[V] =
        import m.{given, *}
        m.getEnv map (env => lattice.injectClosure(lam, env.restrictTo(lam.fv).as[Address]))

    private def evalLiteralValue(exp: SchemeValue): A[V] =
        import m.given
        exp.value match
            // case sexp.Value.String(s)    => storeVal(exp, inject[String, V](s))
            case sexp.Value.Integer(n)   => inject(n).pure
            case sexp.Value.Real(r)      => inject(r).pure
            case sexp.Value.Boolean(b)   => inject(b).pure
            case sexp.Value.Character(c) => inject(c).pure
            case sexp.Value.Symbol(s)    => inject(s).pure
            case sexp.Value.Nil          => inject[SimpleSchemeValue, V](SchemeNil).pure

    private def evalVariable(vrb: Var): A[V] =
        import m.{given, *}
        for
            adr <- m.lookupEnv(vrb)
            vlu <- m.lookupSto(adr)
        yield vlu

    private def evalSequence(eps: Iterable[Exp]): A[V] =
        import m.{given, *}
        eps match
            case Nil          => inject[SimpleSchemeValue, Val](SchemeVoid).pure
            case last :: Nil  => m.eval(last)
            case next :: rest => m.eval(next) >> evalSequence(rest)

    private def evalIf(prd: Exp, csq: Exp, alt: Exp): A[V] =
        for
            cnd <- m.eval(prd)
            res <- cond(cnd, m.eval(csq), m.eval(alt))
        yield res

    private def evalLet(
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      ): A[V] =
        import m.{given, *}
        val (vrs, rhs) = bds.unzip
        for
            vls <- evalAll(rhs)
            ads <- vrs.traverse(m.allocVar)
            res <- m.withExtendedEnv(vrs.map(_.name).zip(ads)) {
                ads.zip(vls).traverse_ { case (adr, vlu) => extendSto(adr, vlu) } >> evalSequence(bdy)
            }
        yield res

    private def evalLetStar(
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      ): A[V] =
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

    private def evalLetrec(
        bds: List[(Var, Exp)],
        bdy: List[Exp]
      ): A[V] =
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
    private def evalAssert(exp: Exp): A[V] =
        import m.given
        inject[SimpleSchemeValue, V](SchemeVoid).pure

    private def cond(cnd: V, csq: A[V], alt: A[V]): A[V] =
        import m.{given, *}
        m.cond(cnd) /* then */ { csq } /* else */ { alt }

    private def evalCall(app: App): A[V] =
        import m.{given, *}
        for
            fun <- m.eval(app.f)
            ags <- evalAll(app.args)
            res <- applyFun(app, fun, ags)
        yield res

    private def applyFun(app: App, fun: V, ags: List[V]): A[V] =
        import m.{given, *}
        m.mfoldMap(lattice.split(fun)) {
            case lattice.primitive(prm, _) =>
                applyPrimitive(app, dom.primitives.allPrimitives(prm), ags)
            case lattice.closures((lam, lex), _) =>
                applyClosure(app, lam, lex, ags)
            case v => raiseError(TypeError("value cannot applied", v))
        }

    private def applyPrimitive(app: App, prm: Prim, ags: List[V]): A[V] =
        prm.call(app, ags)

    private def applyClosure(
        app: App,
        lam: Lam,
        lex: Env,
        ags: List[Val]
      ): A[Val] =
        import m.{given, *}
        val agc = ags.length
        for
            _ <- guard(lam.check(agc)) {
                raiseError(ArityError(lam.idn, lam.args.size, agc, app.idn))
            }
            fvs <- lex.addrs.toList.traverse { case adr: ValAddress[V @unchecked] =>
                lookupSto(adr).map((adr, _))
            }
            result <- withCtx(newContext(app, lam, ags, _)) {
                argBindings(app, lam, ags, fvs) >>= { bds =>
                    val envBds = bds.map((nam, adr, _) => (nam, adr))
                    val stoBds = bds.map((_, adr, vlu) => (adr, vlu))
                    withEnv(_ => Environment(envBds)) {
                        stoBds.traverse_ { case (adr, vlu) => extendSto(adr, vlu) } >> call(lam)
                    }
                }
            }
        yield result

    private def argBindings(
        app: App,
        lam: Lam,
        ags: List[Val],
        fvs: Iterable[(Adr, Val)]
      ): A[List[(String, ValAddress[Val], Val)]] =
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
                        lst <- allocList(vex, vag)
                        adr <- allocVar(varArg)
                    yield List((varArg.name, adr, lst))
            // free variables
            frv <- fvs.toList.traverse { (adr, vlu) =>
                adr match
                    case VarAddrWithContext(idf, _) => allocVar(idf).map((idf.name, _, vlu))
                    case adr: PrmAddr[V @unchecked] => (adr.nam, adr: ValAddress[V], vlu).pure
            }
        yield fxa ++ vra ++ frv
