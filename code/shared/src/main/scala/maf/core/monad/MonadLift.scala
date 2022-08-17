package maf.core.monad

trait MonadLift[T[_[_]]]:
    def lift[M[_], A](m: M[A]): T[M]

object MonadLift:
    def lift[M[_], T[_[_]], A](m: M[A])(using ml: MonadLift[T]): T[M] =
        ml.lift(m)
