package maf.util

object TailrecUtil:
    import scala.util.control.TailCalls._

    def sequence[A](tailrecs: List[TailRec[A]]): TailRec[List[A]] = tailrecs match
        case List() => done(List())
        case x :: xs =>
          for
              v <- x
              vs <- sequence(xs)
          yield (v :: vs)
