package maf.util

// Wrapper

trait Wrapper[+C[_]]:
    type T
    lazy val instance: C[T]

object Wrapper:
    given wrapper[X, C[_]](using => C[X]): Wrapper[C] with
        type T = X
        lazy val instance = summon
    given instance[C[_]](using w: Wrapper[C]): C[w.T] = w.instance

// Wrapper2

trait Wrapper2[+C[_,_]]:
    type T1
    type T2
    lazy val instance: C[T1, T2]

object Wrapper2:
    given wrapper[X, Y, C[_, _]](using => C[X, Y]): Wrapper2[C] with
        type T1 = X
        type T2 = Y
        lazy val instance = summon
    given instance[C[_, _]](using w: Wrapper2[C]): C[w.T1, w.T2] = w.instance