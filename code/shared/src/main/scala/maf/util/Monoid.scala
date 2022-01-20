package maf.util

import maf.core._
import maf.core.worklist.WorkList
import maf.util.datastructures._

trait Monoid[M] extends Serializable:
    def append(x: M, y: => M): M
    def zero: M

object Monoid:
    def apply[M: Monoid]: Monoid[M] = implicitly

    given listMonoid[T]: Monoid[List[T]] with
        def append(x: List[T], y: => List[T]): List[T] = x ++ y
        def zero: List[T] = List()

object MonoidImplicits:
    implicit class FoldMapExtension[X](coll: Iterable[X]):
        def foldMap[M: Monoid](f: X => M): M =
          coll.foldLeft(Monoid[M].zero)((acc, elm) => Monoid[M].append(acc, f(elm)))
    implicit def setMonoid[X]: Monoid[Set[X]] = MonoidInstances.setMonoid
    implicit def mapMonoid[K, V: Monoid]: Monoid[Map[K, V]] = MonoidInstances.mapMonoid
    implicit def latticeMonoid[L: Lattice]: Monoid[L] = MonoidInstances.latticeMonoid
    implicit def mayFail[M: Monoid]: Monoid[MayFail[M, Error]] = MonoidInstances.mayFail

object MonoidInstances:
    def latticeMonoid[L: Lattice]: Monoid[L] = new Monoid[L] {
      def append(x: L, y: => L): L = Lattice[L].join(x, y)
      def zero: L = Lattice[L].bottom
    }
    def mayFail[M](implicit monoid: Monoid[M]): Monoid[MayFail[M, Error]] =
      new Monoid[MayFail[M, Error]] {
        def append(x: MayFail[M, Error], y: => MayFail[M, Error]): MayFail[M, Error] = (x, y) match
            case (MayFailSuccess(x), MayFailSuccess(y))       => MayFailSuccess(monoid.append(x, y))
            case (MayFailSuccess(x), MayFailError(errs))      => MayFailBoth(x, errs)
            case (MayFailSuccess(x), MayFailBoth(y, errs))    => MayFailBoth(monoid.append(x, y), errs)
            case (MayFailError(errs), MayFailSuccess(x))      => MayFailBoth(x, errs)
            case (MayFailError(errs1), MayFailError(errs2))   => MayFailError(errs1 ++ errs2)
            case (MayFailError(errs1), MayFailBoth(x, errs2)) => MayFailBoth(x, errs1 ++ errs2)
            case (MayFailBoth(x, errs), MayFailSuccess(y))    => MayFailBoth(monoid.append(x, y), errs)
            case (MayFailBoth(x, errs1), MayFailError(errs2)) => MayFailBoth(x, errs1 ++ errs2)
            case (MayFailBoth(x, errs1), MayFailBoth(y, errs2)) =>
              MayFailBoth(monoid.append(x, y), errs1 ++ errs2)
        def zero: MayFail[M, Error] = MayFailSuccess(monoid.zero)
      }
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
    def workListMonoid[M]: Monoid[WorkList[M]] = new Monoid[WorkList[M]] {
      def zero: WorkList[M] = WorkList.empty
      def append(x: WorkList[M], y: => WorkList[M]): WorkList[M] = x.addAll(y.toList)
    }
    def mutliSetSumMonoid[X] = new Monoid[MultiSet[X]] {
      def zero = MultiSet.empty
      def append(x: MultiSet[X], y: => MultiSet[X]) = x ++ y
    }
    def multiSetMaxMonoid[X] = new Monoid[MultiSet[X]] {
      def zero = MultiSet.empty
      def append(x: MultiSet[X], y: => MultiSet[X]) = x.combine(y)(Math.max)
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
