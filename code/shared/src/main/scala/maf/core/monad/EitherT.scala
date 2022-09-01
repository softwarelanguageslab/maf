package maf.core.monad

import maf.core.Monad
import maf.core.Monad.*

// data EitherT = EitherT { runEither :: m (Either e a) }
case class EitherT[E, M[_]: Monad, A](runEither: M[Either[E, A]])

// Curried version of EitherT
type EitherT_ = [E] =>> [M[_]] =>> [A] =>> EitherT[E, M, A]

object EitherT:
    given [E, M[_]: Monad]: Monad[EitherT_[E][M]] with
        type A[X] = EitherT_[E][M][X]
        def unit[X](x: X): A[X] =
            EitherT(Monad[M].unit(Right(x)))
        def flatMap[X, Y](m: A[X])(f: X => A[Y]): A[Y] =
            EitherT(m.runEither.flatMap {
                case Left(v)  => Monad[M].unit(Left[E, Y](v))
                case Right(x) => f(x).runEither
            })

        def map[X, Y](m: EitherT_[E][M][X])(f: X => Y): EitherT_[E][M][Y] =
            flatMap(m)(f andThen unit)
