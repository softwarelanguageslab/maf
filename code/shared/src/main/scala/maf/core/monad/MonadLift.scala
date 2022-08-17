package maf.core.monad

import maf.core.Monad

trait MonadLift[T[_[_], _]]:
    def lift[M[_]: Monad, A](m: M[A]): T[M, A]

object MonadLift:
    def lift[M[_]: Monad, T[_[_], _], A](m: M[A])(using ml: MonadLift[T]): T[M, A] =
        ml.lift(m)
