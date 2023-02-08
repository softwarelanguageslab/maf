package maf.modular.scheme.monadic

import maf.core.Monad
import maf.core.Monad.*
import maf.modular.scheme.modflocal.*
import maf.modular.scheme.SchemeDomain
import maf.core.MaybeEq
import maf.core.Lattice
import maf.core.monad.{MonadLift, MonadUnlift}

trait EnvT extends SchemeSemantics, StubAnalysisM, SchemeDomain, SchemeModFLocalSensitivity:
    outer =>
    case class EnvT_[M[_], A](runEnv: (Env, Ctx) => M[A])
    type EnvT[M[_]] = [A] =>> EnvT_[M, A]

    protected given envMonad[M[_]: Monad]: Monad[EnvT[M]] with
        def unit[X](x: X): EnvT[M][X] = EnvT_((_, _) => Monad[M].unit(x))
        def flatMap[X, Y](m: EnvT[M][X])(f: X => EnvT[M][Y]): EnvT[M][Y] =
            EnvT_((e, ctx) => m.runEnv(e, ctx).flatMap(f andThen (_.runEnv(e, ctx))))
        def map[X, Y](m: EnvT[M][X])(f: X => Y): EnvT[M][Y] =
            flatMap(m)(f andThen unit)

    protected given ml: MonadLift[EnvT_] with
        def lift[M[_]: Monad, A](m: M[A]): EnvT_[M, A] =
            EnvT_((_, _) => m)

    protected given ul: MonadUnlift[EnvT_] with
        def unlift[M[_]: Monad, A, B](t: EnvT_[M, A])(f: M[A] => EnvT_[M, B]): EnvT_[M, B] = EnvT_((e, ctx) => f(t.runEnv(e, ctx)).runEnv(e, ctx))

    /** An instance of the analysisM monad */
    given envTM[M[_]](using next: AnalysisM[M]): AnalysisM[EnvT[M]] with
        private type A[X] = EnvT[M][X]
        private val stub: AnalysisM[EnvT[M]] = stubAnalysisM(using ml, ul, next, envMonad)
        export stub.{getCtx => _, getEnv => _, withCtx => _, withEnv => _, *}

        override def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            EnvT_((e, ctx) => blk.runEnv(f(e), ctx))
        override def getEnv: A[Env] =
            EnvT_((e, _) => Monad[M].unit(e))
        override def getCtx: A[Ctx] =
            EnvT_((_, ctx) => Monad[M].unit(ctx))
        override def withCtx[X](f: Ctx => Ctx)(blk: A[X]): A[X] =
            EnvT_((e, ctx) => blk.runEnv(e, f(ctx)))
