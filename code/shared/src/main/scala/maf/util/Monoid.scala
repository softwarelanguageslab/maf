package maf.util

import maf.core._
import maf.core.worklist.WorkList

trait Monoid[M] {
  def append(x: M, y: => M): M
  def zero: M
}

object Monoid {
  def apply[M: Monoid]: Monoid[M] = implicitly
}

object MonoidImplicits {
  implicit class FoldMapExtension[X](coll: Iterable[X]) {
      def foldMap[M : Monoid](f: X => M): M =
        coll.foldLeft(Monoid[M].zero)((acc,elm) => Monoid[M].append(acc,f(elm)))
  }
  implicit def setMonoid[X]: Monoid[Set[X]] = MonoidInstances.setMonoid
  implicit def latticeMonoid[L : Lattice]: Monoid[L] = MonoidInstances.latticeMonoid
  implicit def mayFail[M : Monoid]: Monoid[MayFail[M,Error]] = MonoidInstances.mayFail
}

object MonoidInstances {
  def latticeMonoid[L: Lattice]: Monoid[L] = new Monoid[L] {
    def append(x: L, y: => L): L = Lattice[L].join(x, y)
    def zero: L                  = Lattice[L].bottom
  }

  def mayFail[M](implicit monoid: Monoid[M]): Monoid[MayFail[M, Error]] =
    new Monoid[MayFail[M, Error]] {
      def append(x: MayFail[M, Error], y: => MayFail[M, Error]): MayFail[M, Error] = (x, y) match {
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
      }
      def zero: MayFail[M, Error] = MayFailSuccess(monoid.zero)
    }

  def combineAll[A: Monoid](list: List[A]): A =
    list.foldLeft(Monoid[A].zero)((a, b) => Monoid[A].append(a, b))

  def combineAllMap[A, B: Monoid](f: A => B)(list: List[A]): B =
    list.map(f).foldLeft(Monoid[B].zero)((a, b) => Monoid[B].append(a, b))

  def combineAllNonEmpty[A: Monoid](list: List[A]): A =
    list.tail.foldLeft(list.head)((a, b) => Monoid[A].append(a, b))

  implicit def setMonoid[M]: Monoid[Set[M]] = new Monoid[Set[M]] {
    def append(x: Set[M], y: => Set[M]): Set[M] = x ++ y
    def zero: Set[M]                            = Set[M]()
  }

  def workListMonoid[M]: Monoid[WorkList[M]] = new Monoid[WorkList[M]] {
    def zero: WorkList[M] = WorkList.empty
    def append(x: WorkList[M], y: => WorkList[M]): WorkList[M] = x.addAll(y.toList)
  }
  val boolOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x || y
    def zero: Boolean                              = false
  }
  val boolAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def append(x: Boolean, y: => Boolean): Boolean = x && y
    def zero: Boolean                              = true
  }
  implicit def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def append(x: Option[A], y: => Option[A]): Option[A] = (x, y) match {
        case (Some(a), Some(b)) => Some(Monoid[A].append(a, b))
        case (_, _) => None
      }

    def zero: Option[A] = None
  }
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def append(x: String, y: => String): String = x ++ " " ++ y
    def zero: String = ""
  }
}
