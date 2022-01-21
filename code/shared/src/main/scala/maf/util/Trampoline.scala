package maf.util

import maf.core.{IdentityMonad, Monad}
import scala.annotation.tailrec

class TrampolineT[M[_], T]
case class More[M[_], T](next: () => TrampolineT[M, T]) extends TrampolineT[M, T]:
    def execute: TrampolineT[M, T] = next()
case class Done[M[_], T](value: M[T]) extends TrampolineT[M, T]
case class FlatMap[M[_], A, B](sub: TrampolineT[M, A], cnt: A => TrampolineT[M, B]) extends TrampolineT[M, B]

object TrampolineT:
    type TrampolineM[M[_]] = [T] =>> TrampolineT[M, T]
    given trampoline[M[_]: Monad]: Monad[TrampolineM[M]] with
        def unit[A](v: A): TrampolineT[M, A] = Done(Monad[M].unit(v))
        def flatMap[A, B](m: TrampolineT[M, A])(f: A => TrampolineT[M, B]): TrampolineT[M, B] =
          FlatMap(m, f)
        def map[A, B](m: TrampolineT[M, A])(f: A => B): TrampolineT[M, B] =
          flatMap(m)(x => unit(f(x)))

object Trampoline:
    def done[M[_]: Monad, T](v: T): TrampolineT[M, T] = Done(Monad[M].unit(v))

    def tailcall[M[_], T](v: => TrampolineT[M, T]): TrampolineT[M, T] =
      More(() => v)

    @tailrec
    def run[T](trampoline: TrampolineT[IdentityMonad.Id, T]): IdentityMonad.Id[T] =
      trampoline match
          case Done(v) => v
          case More(f) => run(f())
          case FlatMap((m: TrampolineT[IdentityMonad.Id, T]), (f: (T => TrampolineT[IdentityMonad.Id, T]))) =>
            m match {
              case Done(v: IdentityMonad.Id[T]) => run(f(v))
              case More(k)                      => run(FlatMap(k(), f))
              case FlatMap(sub2, cont2) =>
                run(FlatMap(sub2, (x) => FlatMap(cont2(x), f)))
            }

    def lift[M[_]: Monad, A](m: M[A]): TrampolineT[M, A] =
      Done(m)

type Trampoline[T] = TrampolineT[IdentityMonad.Id, T]
