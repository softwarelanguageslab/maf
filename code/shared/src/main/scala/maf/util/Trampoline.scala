package maf.util

import maf.core.Monad

class Trampoline[+T]
case class More[T](next: () => Trampoline[T]) extends Trampoline[T]:
    def execute: Trampoline[T] = next()
case class Done[T](value: T) extends Trampoline[T]
case class FlatMap[A, B](sub: Trampoline[A], cnt: A => Trampoline[B]) extends Trampoline[B]

object Trampoline:
    given Monad[Trampoline] with
        def unit[A](v: A): Trampoline[A] = Done(v)
        def flatMap[A, B](m: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] =
          FlatMap(m, f)
        def map[A, B](m: Trampoline[A])(f: A => B): Trampoline[B] =
          flatMap(m)(x => unit(f(x)))

    def done[T](v: T): Trampoline[T] = Done(v)

    def tailcall[T](v: => Trampoline[T]): Trampoline[T] =
      More(() => v)

    def run[T](trampoline: Trampoline[T]): T =
      trampoline match
          case Done(v) => v
          case More(f) => run(f())
          case FlatMap(m, f) =>
            m match {
              case Done(v) => run(f(v))
              case More(k) => run(FlatMap(k(), f))
              case FlatMap(sub2, cont2) =>
                run(FlatMap(sub2, (x) => FlatMap(cont2(x), f)))
            }
