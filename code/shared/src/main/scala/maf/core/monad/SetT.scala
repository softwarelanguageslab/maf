package maf.core.monad

import maf.core.Monad
import maf.core.Monad.*
import maf.util.Monoid.*
import maf.util.MonoidImplicits.*

// data SetT m a = SetT { runSet :: m [a] }
case class SetT[M[_]: Monad, X](runSet: M[Set[X]])

// curried version of SetT
type SetT_ = [M[_]] =>> [X] =>> SetT[M, X]

object SetT:
    def empty[T[_]: Monad, X]: SetT[T, X] =
        SetT(Monad[T].unit(Set()))

    given mSetT[T[_]: Monad]: Monad[SetT_[T]] with
        type M[X] = SetT_[T][X]
        def unit[X](x: X): SetT_[T][X] =
            SetT(Monad[T].unit(Set(x)))
        def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] = SetT(for
            a <- m.runSet
            b <- a.mapM(f andThen (_.runSet)).map(_.mconcat)
        yield b)
        def map[X, Y](m: SetT_[T][X])(f: X => Y): SetT_[T][Y] =
            flatMap(m)(f andThen unit)

    given lSetT: MonadLift[SetT] with
        def lift[M[_]: Monad, A](m: M[A]): SetT[M, A] =
            SetT(m.map(Set(_)))

    given ulSetT: MonadUnlift[SetT] with
        def unlift[M[_$8]: Monad, A, B](t: SetT[M, A])(f: M[A] => SetT[M, B]): SetT[M, B] = ???
