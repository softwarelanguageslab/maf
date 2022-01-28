package maf.core

import maf.core.IdentityMonad.Id

//
// Monad
//

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
        def >>>[Y](m: => M[Y]): M[Y] = flatMap(_ => m)
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
          else f(xs.head) >>> { xs.tail.mapM_(f) }

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

    extension [M[_]: Monad, X](xs: Iterable[M[Set[X]]])
      def flattenM: M[Set[X]] =
        xs.foldSequence(Set())((all, rest) => Monad[M].unit(all ++ rest))

    def sequence[M[_]: Monad, X](ms: List[M[X]]): M[List[X]] =
        import maf.core.Monad.MonadSyntaxOps
        ms match
            case m :: rest =>
              for
                  v <- m
                  vs <- sequence(rest)
              yield v :: vs
            case Nil => Monad[M].unit(List())

    given idMonad: Monad[Id] with
        def unit[T](v: T) = v
        def flatMap[A, B](m: Id[A])(f: A => Id[B]): Id[B] =
          f(m)
        def map[A, B](m: Id[A])(f: A => B): Id[B] =
          flatMap(m)((a) => unit(f(a)))

    /** For any Monad M provides a way to merge a set of such monads into a single monad-wrapped value using the join of the given lattice */
    def merge[X: Lattice, M[_]: Monad](xs: List[M[X]]): M[X] = xs match
        case List() => Monad[M].unit(Lattice[X].bottom)
        case x :: rest =>
          for
              v <- x
              vs <- merge(xs)
          yield Lattice[X].join(v, vs)

//
// MonadError
//

trait MonadError[M[_], E] extends Monad[M]:
    def fail[X](err: E): M[X]

//
// MonadJoin
//

trait MonadJoin[M[_]] extends Monad[M]:
    def mbottom[X]: M[X]
    def mjoin[X: Lattice](x: M[X], y: M[X]): M[X]
    // for convenience
    def mjoin[X: Lattice](xs: Iterable[M[X]]): M[X] =
      xs.foldLeft(mbottom: M[X])((acc, m) => mjoin(acc, m))
    def mfold[X: Lattice](xs: Iterable[X]): M[X] =
      mjoin(xs.map(unit))
    def mfoldMap[X, Y: Lattice](xs: Iterable[X])(f: X => M[Y]): M[Y] =
      mjoin(xs.map(f))
    def guard(cnd: Boolean): M[Unit] =
      if cnd then unit(()) else mbottom
    def withFilter[X](m: M[X])(p: X => Boolean): M[X] =
      flatMap(m)(x => map(guard(p(x)))(_ => x))
    def inject[X: Lattice](x: X): M[X] =
      map(guard(!Lattice[X].isBottom(x)))(_ => x)

object MonadJoin:
    // to easily access an implicit Monad instance
    def apply[M[_]: MonadJoin]: MonadJoin[M] = implicitly
    // nicer syntax in Scala
    implicit class MonadJoinSyntaxOps[M[_]: MonadJoin, X](self: M[X]):
        def ++(other: M[X])(implicit ev: Lattice[X]): M[X] = MonadJoin[M].mjoin(self, other)
        def withFilter(p: X => Boolean): M[X] = MonadJoin[M].withFilter(self)(p)
    implicit class MonadJoinIterableSyntax[X](xs: Iterable[X]):
        def foldMapM[M[_]: MonadJoin, Y: Lattice](f: X => M[Y]): M[Y] = MonadJoin[M].mfoldMap(xs)(f)

///
/// MonadStateT
///

trait StateOps[S, M[_]] extends Monad[M]:
    def get: M[S]
    def put(snew: S): M[Unit]
    def withState[X](f: S => S)(m: M[X]): M[X]
    def impure[X](f: => X): M[X]

class MonadStateT[S, M[_]: Monad, A](val run: S => M[(A, S)]):
    def runValue(init: S) = Monad[M].map(run(init))(_._1)

object MonadStateT:
    import maf.core.Monad.MonadSyntaxOps
    def apply[S, M[_]: Monad, A](run: S => M[(A, S)]): MonadStateT[S, M, A] =
      new MonadStateT(run)

    given stateInstance[S, M[_]: Monad]: StateOps[S, [A] =>> MonadStateT[S, M, A]] with
        private type SM[A] = MonadStateT[S, M, A]
        def unit[X](x: X): SM[X] =
          MonadStateT((s: S) => Monad[M].unit((x, s)))
        def map[X, Y](m: SM[X])(f: X => Y): SM[Y] =
          MonadStateT((s: S) =>
            Monad[M].map(m.run(s)) { case (v, snew) =>
              (f(v), snew)
            }
          )

        def flatMap[X, Y](m: SM[X])(f: X => SM[Y]): SM[Y] =
          MonadStateT((s: S) =>
            Monad[M].flatMap(m.run(s)) { case (v, snew) =>
              f(v).run(snew)
            }
          )

        def get: SM[S] = MonadStateT((s: S) => Monad[M].unit((s, s)))
        def put(snew: S): SM[Unit] = MonadStateT((s: S) => Monad[M].unit(((), snew)))
        def impure[X](f: => X): SM[X] = MonadStateT((s: S) => Monad[M].unit((f, s)))

        def withState[X](f: S => S)(m: SM[X]): SM[X] =
          for
              oldState <- get
              _ <- put(f(oldState))
              result <- m
              _ <- put(oldState)
          yield result

    def lift[S, M[_]: Monad, X](v: M[X]): MonadStateT[S, M, X] = MonadStateT((s: S) => Monad[M].map(v)((_, s)))
    def unlift[S, M[_]: Monad, X](ms: MonadStateT[S, M, X]): MonadStateT[S, M, M[X]] =
      MonadStateT((s: S) => {
        val res = ms.run(s)
        val innerRes = Monad[M].map(res) { case (v, snew) =>
          v
        }
        Monad[M].flatMap(res) { case (v, snew) =>
          Monad[M].unit((innerRes, snew))
        }
      })

///
/// SetMonad
///

/** This simply provides a functional interface for the existing Scala Set monad */
object SetMonad:
    implicit def iterableMonadInstance: Monad[[A] =>> Set[A]] = new Monad:
        type M[A] = Set[A]
        def unit[X](x: X): M[X] = Set(x)
        def map[X, Y](m: M[X])(f: X => Y): M[Y] =
          m.map(f)
        def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] =
          m.flatMap(f)

    def fail[A]: Set[A] = Set()

///
/// Option Monad
///

object OptionMonad:
    given Monad[Option] = new Monad:
        def unit[X](x: X): Option[X] = Some(x)
        def map[X, Y](m: Option[X])(f: X => Y): Option[Y] =
          m.map(f)
        def flatMap[X, Y](m: Option[X])(f: X => Option[Y]): Option[Y] =
          m.flatMap(f)

///
/// Identity Monad
///

object IdentityMonad:
    type Id[X] = X
    export Monad.idMonad
