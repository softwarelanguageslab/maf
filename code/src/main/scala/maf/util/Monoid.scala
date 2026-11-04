package maf.util

trait Monoid[M] extends Serializable:
    def append(x: M, y: => M): M
    def zero: M

object Monoid:
    def apply[M: Monoid]: Monoid[M] = implicitly

    extension [L: Monoid](m: Iterable[L])
        def mconcat: L =
            var result = Monoid[L].zero
            for (x <- m) do result = Monoid[L].append(result, x)
            result

    given listMonoid[T]: Monoid[List[T]] with
        def append(x: List[T], y: => List[T]): List[T] = x ++ y
        def zero: List[T] = List()

object MonoidImplicits:
    implicit class FoldMapExtension[X](coll: Iterable[X]):
        def foldMap[M: Monoid](f: X => M): M =
            coll.foldLeft(Monoid[M].zero)((acc, elm) => Monoid[M].append(acc, f(elm)))
    implicit def setMonoid[X]: Monoid[Set[X]] = MonoidInstances.setMonoid
    implicit def mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] = MonoidInstances.mapMonoid

object MonoidInstances:
    def setMonoid[M]: Monoid[Set[M]] = new Monoid[Set[M]] {
        def append(x: Set[M], y: => Set[M]): Set[M] = x ++ y
        def zero: Set[M] = Set[M]()
    }
    def mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
        def append(x: Map[K, V], y: => Map[K, V]): Map[K, V] =
            y.foldLeft(x) { case (acc, (k, v)) =>
                acc.get(k) match
                    case None     => acc + (k -> v)
                    case Some(v2) => acc + (k -> Monoid[V].append(v, v2))
            }
        def zero: Map[K, V] = Map.empty
    }
    val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def append(x: Boolean, y: => Boolean): Boolean = x || y
        def zero: Boolean = false
    }
    val boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def append(x: Boolean, y: => Boolean): Boolean = x && y
        def zero: Boolean = true
    }
    val intMaxMonoid: Monoid[Int] = new Monoid[Int] {
        def append(x: Int, y: => Int): Int = Math.max(x, y)
        def zero: Int = 0
    }
    val intMinMonoid: Monoid[Int] = new Monoid[Int] {
        def append(x: Int, y: => Int): Int = Math.min(x, y)
        def zero: Int = Int.MaxValue
    }
