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
    case class EnvT_[M[_], A](runEnv: Env => M[A])
    type EnvT[M[_]] = [A] =>> EnvT_[M, A]

    protected given envMonad[M[_]: Monad]: Monad[EnvT[M]] with
        def unit[X](x: X): EnvT[M][X] = EnvT_(_ => Monad[M].unit(x))
        def flatMap[X, Y](m: EnvT[M][X])(f: X => EnvT[M][Y]): EnvT[M][Y] =
            EnvT_(e => m.runEnv(e).flatMap(f andThen (_.runEnv(e))))
        def map[X, Y](m: EnvT[M][X])(f: X => Y): EnvT[M][Y] =
            flatMap(m)(f andThen unit)

    protected given MonadLift[EnvT_] with
        def lift[M[_]: Monad, A](m: M[A]): EnvT_[M, A] =
            EnvT_(_ => m)

    protected given MonadUnlift[EnvT_] with
        def unlift[M[_]: Monad, A, B](t: EnvT_[M, A])(f: M[A] => EnvT_[M, B]): EnvT_[M, B] = EnvT_(e => f(t.runEnv(e)).runEnv(e))

    /** An instance of the analysisM monad */
    given [M[_]: AnalysisM]: AnalysisM[EnvT[M]] with
        private type A[X] = EnvT[M][X]
        private val stub: AnalysisM[EnvT[M]] = stubAnalysisM
        export stub.{getEnv => _, withEnv => _, *}

        override def withEnv[X](f: Env => Env)(blk: A[X]): A[X] =
            EnvT_(e => blk.runEnv(f(e)))
        override def getEnv: A[Env] =
            EnvT_(e => Monad[M].unit(e))
