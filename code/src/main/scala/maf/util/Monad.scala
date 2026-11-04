package maf.util

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

trait Monad[M[_]]:
    def unit[X](x: X): M[X]
    def map[X, Y](m: M[X])(f: X => Y): M[Y]
    def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y]

object Monad:
    // to easily access an implicit Monad instance
    def apply[M[_]: Monad]: Monad[M] = implicitly
    // necessary to get the fancy for-yield syntax in Scala
    implicit class MonadSyntaxOps[M[_]: Monad, X](self: M[X]):
        def map[Y](f: X => Y): M[Y] = Monad[M].map(self)(f)
        def flatMap[Y](f: X => M[Y]): M[Y] = Monad[M].flatMap(self)(f)
        def >>=[Y](f: X => M[Y]): M[Y] = flatMap(f)
        def >>[Y](m: => M[Y]): M[Y] = flatMap(_ => m)

    extension [M[_]: Monad, X, Y, Z](v: (M[X], M[Y]))
        def mapN(f: (X, Y) => Z): M[Z] =
            v._1.flatMap(x => v._2.flatMap(y => Monad[M].unit(f(x, y))))

    // Kleisli Arrow Composition
    extension [M[_]: Monad, X, Y](start: X => M[Y])
        def >=>[Z](end: Y => M[Z]): X => M[Z] =
            (x: X) => start(x).flatMap(end)

    // some common monad operations on iterables
    implicit class MonadIterableOps[X](xs: Iterable[X]):

        def mapM[M[_]: Monad, Y](f: X => M[Y]): M[List[Y]] =
            if xs.isEmpty then Monad[M].unit(Nil)
            else
                for
                    fx <- f(xs.head)
                    rst <- xs.tail.mapM(f)
                yield fx :: rst

        def mapM_[M[_]: Monad, Y](f: X => M[Y]): M[Unit] =
            if xs.isEmpty then Monad[M].unit(())
            else f(xs.head) >> { xs.tail.mapM_(f) }

        def foldLeftM[M[_]: Monad, Y](acc: Y)(f: (Y, X) => M[Y]): M[Y] =
            if xs.isEmpty then Monad[M].unit(acc)
            else f(acc, xs.head) >>= { xs.tail.foldLeftM(_)(f) }

        def foldRightM[M[_]: Monad, Y](nil: Y)(f: (X, Y) => M[Y]): M[Y] =
            if xs.isEmpty then Monad[M].unit(nil)
            else xs.tail.foldRightM(nil)(f) >>= { f(xs.head, _) }

    implicit class MonadSequenceOps[M[_]: Monad, X](xs: Iterable[M[X]]):
        def foldSequence[Y](nil: Y)(f: (X, Y) => M[Y]): M[Y] =
            if xs.isEmpty then Monad[M].unit(nil)
            else
                xs.tail.foldSequence(nil)(f) >>= { rest =>
                    xs.head >>= { head => f(head, rest) }
                }

    /** "if" expressions for monads */
    def mIf[M[_]: Monad, X](m: M[Boolean])(csq: => M[X])(alt: => M[X]): M[X] =
        m.flatMap(b => if b then csq else alt)

    extension [M[_]: Monad, X](xs: Iterable[M[Set[X]]])
        def flattenM: M[Set[X]] =
            xs.foldSequence(Set())((all, rest) => Monad[M].unit(all ++ rest))

    // Compatibility layer to support Tailcall using our monad operations
    given tailcallMonad: Monad[TailRec] with
        private type M[X] = TailRec[X]
        def unit[X](x: X): M[X] =
            TailCalls.done(x)

        def map[X, Y](m: M[X])(f: X => Y): M[Y] =
            m.map(f)

        def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] =
            m.flatMap(f)
