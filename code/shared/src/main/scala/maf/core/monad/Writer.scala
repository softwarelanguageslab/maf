package maf.core.monad

import maf.core.Monad
import maf.util.Monoid

case class Writer[T, A](value: A, contents: T):
    def runWriter: (A, T) = (this.value, this.contents)

object Writer:
    given writerMonad[T: Monoid]: Monad[[A] =>> Writer[T, A]] with
        type M[A] = Writer[T, A]
        def unit[X](x: X): M[X] = Writer(x, Monoid[T].zero)
        def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] =
            val w2 = f(m.value)
            Writer(w2.value, Monoid[T].append(m.contents, w2.contents))

        def map[X, Y](m: M[X])(f: X => Y): M[Y] =
            flatMap(m)(x => unit(f(x)))
