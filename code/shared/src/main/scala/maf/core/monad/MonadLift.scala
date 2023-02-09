package maf.core.monad

import maf.core.Monad

trait MonadLift[T[_[_], _]]:
    def lift[M[_]: Monad, A](m: M[A]): T[M, A]

trait MonadUnlift[T[_[_], _]]:
    def unlift[M[_]: Monad, A, B](t: T[M, A])(f: M[A] => T[M, B]): T[M, B]

object MonadLift:
    def lift[M[_]: Monad, T[_[_], _], A](m: M[A])(using ml: MonadLift[T]): T[M, A] =
        ml.lift(m)

    def unlift[M[_]: Monad, T[_[_], _], A, B](t: T[M, A])(f: M[A] => T[M, B])(using ul: MonadUnlift[T]): T[M, B] = ul.unlift(t)(f)
