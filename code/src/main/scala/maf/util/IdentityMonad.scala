package maf.util

object IdentityMonad:
    type Id[X] = X

    given idMonad: Monad[Id] with
        def unit[T](v: T) = v
        def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] =
            f(m)
        def map[A, B](m: Id[A])(f: A => B): Id[B] =
            flatMap(m)((a) => unit(f(a)))
