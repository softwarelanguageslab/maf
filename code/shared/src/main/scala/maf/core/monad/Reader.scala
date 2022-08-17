package maf.core.monad

import maf.core.Monad
import maf.core.Monad.MonadSyntaxOps
import maf.core.IdentityMonad

case class ReaderT[M[_], T, A](runReader: (T) => M[A])

object ReaderT:
    type Reader[R] = [X] =>> ReaderT[IdentityMonad.Id, R, X]

    given readerMonad[M[_]: Monad, T]: Monad[[A] =>> ReaderT[M, T, A]] with
        type R[A] = ReaderT[M, T, A]
        def unit[X](v: X): R[X] = ReaderT((_: T) => Monad[M].unit(v))
        def flatMap[X, Y](m: R[X])(f: X => R[Y]): R[Y] =
            ReaderT((r) => m.runReader(r).flatMap((x) => f(x).runReader(r)))
        def map[X, Y](m: R[X])(f: X => Y): R[Y] =
            flatMap(m)(f andThen unit)

    def ask[M[_]: Monad, T]: ReaderT[M, T, T] =
        ReaderT((r) => Monad[M].unit(r))

    def local[M[_]: Monad, R, A](f: R => R)(m: ReaderT[M, R, A]): ReaderT[M, R, A] =
        ReaderT((r) => m.runReader(f(r)))

    def lift[M[_], T, A](m: M[A]): ReaderT[M, T, A] =
        ReaderT((_) => m)
