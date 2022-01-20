package maf.util

import scala.reflect.ClassTag

/** An array whose equality is not based on referential equality but on structural equality (i.e., on the elements of the array) */
final class ArrayEq[T: ClassTag](val contents: Array[T]):
    override def equals(that: Any): Boolean = that match
        case e: ArrayEq[_] =>
          contents.zip(e.contents).forall { case (a, b) => a == b }
        case _ => false

    def update(idx: Int, vlu: T): Unit =
      contents(idx) = vlu

    def apply(idx: Int): T =
      contents(idx)

    def map[A: ClassTag](f: T => A): ArrayEq[A] =
      ArrayEq(contents.map(f))

object ArrayEq:
    def from[T: ClassTag](vlus: scala.collection.Iterable[T]): ArrayEq[T] =
      ArrayEq(Array.from(vlus))
