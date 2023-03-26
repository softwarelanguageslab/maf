package maf.values

import maf.values.typeclasses.{BoolLattice, Galois, GaloisFrom}
import maf.util.*
import cats.data.*
import cats.*
import cats.syntax.all._

/** `Show` implementations for common lattice values */
object LatticeShow:
  // Not implicit because it would conflict with stringShow.
  val symShow: Show[String] = new Show[String] {
    def show(s: String): String = s"'$s"
  }

/** Error raised when trying to construct the top element of a lattice which
  * doesn't have one
  */
object LatticeTopUndefined extends MAFException

/** A lattice typeclass. It is actually a join-semi lattice as it only need a
  * join operation and a bottom element
  */
trait Lattice[L] extends PartialOrdering[L] with Show[L] with Serializable:
  /** The value of one of the lattice consituents */
  type SplitVal <: L

  /** A lattice has a bottom element */
  def bottom: L
  def isBottom(x: L): Boolean = x == bottom

  /** A lattice has a top element (might be undefined) */
  def top: L

  /** Elements of the lattice can be joined together
    * @note
    *   Note that the second parameter of join is "by name", which means this
    *   parameter is evaluated upon _every_ use. This has two consequences: (1)
    *   the computation of `y` may be performed multiple times, and (2) the
    *   computation of `y` may not be performed at all, in which case possible
    *   side effects that would be performed are lost.
    */
  def join(x: L, y: => L): L

  /** Joining multiple elements */
  def join(seq: Iterable[L]): L =
    seq.foldLeft(bottom)((acc, elm) => join(acc, elm))

  /** Subsumption between two elements can be checked */
  def subsumes(x: L, y: => L): Boolean

  /** Equality check, returning an abstract result */
  def eql[B: BoolLattice: GaloisFrom[Boolean]](x: L, y: L): B

  /** For PartialOrdering[L]: a lattice has a partial order, defined by
    * subsumes...
    */
  final def lteq(x: L, y: L): Boolean = subsumes(y, x)

  /** Split the value into its constituents such that ∀ V : ⨆ { v ∈ split(V) =
    * V}
    */
  def split(v: L): Set[SplitVal]

  /** ...and elements of the lattice can be compared */
  final def tryCompare(x: L, y: L): Option[Int] =
    (subsumes(x, y), subsumes(y, x)) match
      case (true, true)   => Some(0) // x >= y and y >= x => x = y
      case (true, false)  => Some(1) // x >= y and x != y
      case (false, true)  => Some(-1) // y >= x and x != y
      case (false, false) => None // not comparable

object Lattice:
  def apply[L: Lattice]: Lattice[L] = implicitly

  extension [L](v: L)(using lat: Lattice[L])
    def ⊔(w: => L): L =
      Lattice[L].join(v, w)
    def split: Set[lat.SplitVal] = lat.split(v)

  class SetLattice[A: Show] extends Lattice[Set[A]]:
    type SplitVal = maf.util.datastructures.Singleton[A]
    def show(x: Set[A]): String =
      "{" ++ x.map(Show[A].show _).mkString(",") ++ "}"
    def top = throw LatticeTopUndefined
    def bottom: Set[A] = Set.empty
    def join(x: Set[A], y: => Set[A]): Set[A] = x.union(y)
    def subsumes(x: Set[A], y: => Set[A]): Boolean = y.subsetOf(x)
    def split(v: Set[A]): Set[SplitVal] =
      v.map(maf.util.datastructures.Singleton(_))
    def eql[B: BoolLattice: GaloisFrom[Boolean]](x: Set[A], y: Set[A]) = ???
    def ceq(x: Set[A], y: => Set[A]): Boolean = x == y

  given setLattice[A: Show]: Lattice[Set[A]] = new SetLattice[A]

  implicit object UnitLattice extends Lattice[Unit]:
    type SplitVal = Unit
    def show(v: Unit): String = "()"
    def top = throw LatticeTopUndefined
    def bottom = ()
    def join(x: Unit, y: => Unit): Unit = ()
    def subsumes(x: Unit, y: => Unit): Boolean = true
    def split(v: Unit): Set[Unit] = Set(())
    def eql[B: BoolLattice: GaloisFrom[Boolean]](x: Unit, y: Unit): B =
      Galois.inject[Boolean, B](true)

  def foldMapL[X, L: Lattice](xs: Iterable[X], f: X => L): L =
    if xs.isEmpty then Lattice[L].bottom
    else Lattice[L].join(f(xs.head), foldMapL(xs.tail, f))
