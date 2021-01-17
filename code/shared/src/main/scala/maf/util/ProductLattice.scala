package maf.util

import maf.core.Lattice
import maf.core.MayFail
import maf.core.LatticeTopUndefined
import maf.lattice.interfaces.BoolLattice

/**
 * A partial lattice is the same as a lattice (it must have a subsumes, join, eql, ...)
 * but it does not need to implement all of them for the full abstract domain.
 */
trait PartialLattice[L] {

  /** Elements of the lattice can be joined together */
  def join(x: L, y: => L): L

  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: => L): Boolean

  /** Equality check, returning an abstract result */
  def eql[B: BoolLattice](x: L, y: L): B
}

/**
 * An abstract domain is ordable when each of the values in the abstract domain
 * have an `ord` method indicating their order.
 */
trait Ordable {
  def ord: scala.Int
}

case class ProductLattice[V <: Ordable](vs: List[V]) extends SmartHash {
  override def toString: String =
    if (vs.isEmpty) {
      "⊥"
    } else if (vs.tail.isEmpty) {
      vs.head.toString
    } else {
      vs.map(_.toString).sorted.mkString("{", ",", "}")
    }

  def foldMapL[X](f: V => X)(implicit monoid: Monoid[X]): X =
    vs.foldLeft(monoid.zero)((acc, x) => monoid.append(acc, f(x)))
}

object ProductLattice {
  import MonoidInstances._
  type L[V <: Ordable] = ProductLattice[V]

  def apply[V <: Ordable](v: V): ProductLattice[V] = new ProductLattice(List(v))

  implicit def lMonoid[V <: Ordable](implicit valueLattice: PartialLattice[V]): Monoid[L[V]] = new Monoid[L[V]] {
    private def insert(vs: List[V], v: V): List[V] = vs match {
      case scala.Nil                     => List(v)
      case v0 :: _ if v.ord < v0.ord     => v :: vs
      case v0 :: rest if v.ord == v0.ord => valueLattice.join(v, v0) :: rest
      case v0 :: rest                    => v0 :: insert(rest, v)
    }
    def append(x: L[V], y: => L[V]): L[V] = (x, y) match {
      case (ProductLattice(as), ProductLattice(bs)) => ProductLattice(bs.foldLeft(as)(insert))
    }
    def zero: L[V] = ProductLattice(scala.Nil)
  }

  implicit def lMFMonoid[V <: Ordable](implicit valueLattice: PartialLattice[V]): Monoid[MayFail[L[V], maf.core.Error]] =
    MonoidInstances.mayFail(lMonoid[V])

  implicit def productLattice[V <: Ordable](implicit valueLattice: PartialLattice[V]): Lattice[ProductLattice[V]] = new Lattice[ProductLattice[V]] {
    def show(v: ProductLattice[V]): String = v.toString // TODO[easy]: implement better

    def bottom: ProductLattice[V] = ProductLattice(List())

    def top: ProductLattice[V] = throw LatticeTopUndefined

    def join(x: ProductLattice[V], y: => ProductLattice[V]): ProductLattice[V] =
      Monoid[ProductLattice[V]].append(x, y)

    def subsumes(x: ProductLattice[V], y: => ProductLattice[V]): Boolean =
      y.foldMapL(y =>
        /* For every element in y, there exists an element of x that subsumes it */
        x.foldMapL(x => valueLattice.subsumes(x, y))(boolOrMonoid)
      )(boolAndMonoid)

    def eql[B: BoolLattice](x: ProductLattice[V], y: ProductLattice[V]): B = ??? // TODO[medium] implement
  }
}
