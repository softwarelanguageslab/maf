package maf.values

import maf.values.typeclasses.{BoolLattice, Galois, GaloisFrom}
import maf.util.*
import cats.data.*
import maf.util.datastructures.ListOps.*
import cats.*
import cats.syntax.all._

/** `Show` implementations for common lattice values */
object LatticeShow:
    // Not implicit because it would conflict with stringShow.
    val symShow: Show[String] = new Show[String] {
        def show(s: String): String = s"'$s"
    }

/** Error raised when trying to construct the top element of a lattice which doesn't have one
  */
object LatticeTopUndefined extends MAFException

/** A lattice typeclass. It is actually a join-semi lattice as it only need a join operation and a bottom element
  */
trait Lattice[L] extends PartialOrdering[L] with Show[L] with Serializable:
    /** A lattice has a bottom element */
    def bottom: L
    def isBottom(x: L): Boolean = x == bottom

    /** A lattice has a top element (might be undefined) */
    def top: L

    /** Elements of the lattice can be joined together
      * @note
      *   Note that the second parameter of join is "by name", which means this parameter is evaluated upon _every_ use. This has two consequences:
      *   (1) the computation of `y` may be performed multiple times, and (2) the computation of `y` may not be performed at all, in which case
      *   possible side effects that would be performed are lost.
      */
    def join(x: L, y: => L): L

    /** Joining multiple elements */
    def join(seq: Iterable[L]): L =
        seq.foldLeft(bottom)((acc, elm) => join(acc, elm))

    /** Subsumption between two elements can be checked */
    def subsumes(x: L, y: => L): Boolean

    /** Equality check, returning an abstract result */
    def eql[B: BoolLattice: GaloisFrom[Boolean]](x: L, y: L): B

    /** For PartialOrdering[L]: a lattice has a partial order, defined by subsumes...
      */
    final def lteq(x: L, y: L): Boolean = subsumes(y, x)

    /** Split the value into its constituents such that ∀ V : ⨆ { v ∈ split(V) = V}
      */
    def split(v: L): Set[L]

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
        def ⊑(w: => L): Boolean =
            Lattice[L].subsumes(w, v)
        def ≃[B: BoolLattice: GaloisFrom[Boolean]](w: => L): B =
            Lattice[L].eql(v, w)
        def split: Set[L] = lat.split(v)

    class SetLattice[A: Show] extends Lattice[Set[A]]:
        def show(x: Set[A]): String =
            "{" ++ x.map(Show[A].show _).mkString(",") ++ "}"
        def top = throw LatticeTopUndefined
        def bottom: Set[A] = Set.empty
        def join(x: Set[A], y: => Set[A]): Set[A] = x.union(y)
        def subsumes(x: Set[A], y: => Set[A]): Boolean = y.subsetOf(x)
        def split(v: Set[A]): Set[Set[A]] = v.map(Set(_))
        def eql[B: BoolLattice: GaloisFrom[Boolean]](x: Set[A], y: Set[A]) = ???
        def ceq(x: Set[A], y: => Set[A]): Boolean = x == y

    given setLattice[A: Show]: Lattice[Set[A]] = new SetLattice[A]

    /** Lattice instance for wrapped values */
    def wrapLattice[V: Lattice, W](using self: Iso[V, W]): Lattice[W] = new Lattice[W] {
        def join(x: W, y: => W): W = self.wrap(self.unwrap(x) ⊔ self.unwrap(y))
        def subsumes(x: W, y: => W): Boolean =
            self.unwrap(y) ⊑ self.unwrap(x)
        def show(t: W): String =
            Lattice[V].show(self.unwrap(t))
        def bottom: W = self.wrap(Lattice[V].bottom)
        override def isBottom(x: W): Boolean =
            Lattice[V].isBottom(self.unwrap(x))
        def eql[B: BoolLattice: GaloisFrom[Boolean]](x: W, y: W): B =
            self.unwrap(x) ≃ self.unwrap(y)
        def split(v: W): Set[W] =
            self.unwrap(v).split.map(self.wrap)
        def top: W =
            self.wrap(Lattice[V].top)
    }

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

    /** A lattice implementation for a pair of values */
    given [X: Lattice, Y: Lattice]: Lattice[(X, Y)] with
        def show(v: (X, Y)): String =
            s"(${Lattice[X].show(v._1)}, ${Lattice[Y].show(v._2)})"
        def top = throw LatticeTopUndefined
        def bottom = (Lattice[X].bottom, Lattice[Y].bottom)
        def join(x: (X, Y), y: => (X, Y)): (X, Y) =
            ((x._1 ⊔ y._1), (x._2 ⊔ y._2))
        def subsumes(x: (X, Y), y: => (X, Y)): Boolean =
            (y._1 ⊑ x._1) && (y._2 ⊑ x._2)
        def split(v: (X, Y)): Set[(X, Y)] =
            Lattice[X].split(v._1).cartesian(Lattice[Y].split(v._2)).toSet
        def eql[B: BoolLattice: GaloisFrom[Boolean]](x: (X, Y), y: (X, Y)): B = ???

    given [X: Lattice]: Lattice[Option[X]] with
        def show(v: Option[X]): String =
            v.map(Lattice[X].show).toString()
        def top = throw LatticeTopUndefined
        def bottom = None
        override def isBottom(x: Option[X]): Boolean =
            x.map(v => Lattice[X].isBottom(v)).getOrElse(false)
        def join(x: Option[X], y: => Option[X]): Option[X] = (x, y) match
            case (Some(x), None)    => if Lattice[X].isBottom(x) then None else Some(x)
            case (None, Some(x))    => if Lattice[X].isBottom(x) then None else Some(x)
            case (Some(x), Some(y)) => Some(x ⊔ y)
            case _                  => None
        def subsumes(x: Option[X], y: => Option[X]): Boolean = (x, y) match
            case (Some(_), None)    => true
            case (Some(x), Some(y)) => y ⊑ x
            case _                  => false
        def split(v: Option[X]): Set[Option[X]] =
            v.map(Lattice[X].split(_).map(Some(_))).getOrElse(Set())
        def eql[B: BoolLattice: GaloisFrom[Boolean]](x: Option[X], y: Option[X]): B = ???

    def foldMapL[X, L: Lattice](xs: Iterable[X], f: X => L): L =
        if xs.isEmpty then Lattice[L].bottom
        else Lattice[L].join(f(xs.head), foldMapL(xs.tail, f))

/** A wrapper is an isomorphism from a wrapped value to a wrapper. */
trait Iso[V, W]:
    type Vlu = V
    type Wra = W

    given wrap: Conversion[Vlu, Wra]
    given unwrap: Conversion[Wra, Vlu]

    /** Convenience conversion to convert any value in a monad to a wrapped variant */
    given wrapM[M[_]: Monad]: Conversion[M[Vlu], M[Wra]] =
        (v: M[Vlu]) => v map wrap

    /** Opposite of `wrapM` */
    given unwrapM[M[_]: Monad]: Conversion[M[Wra], M[Vlu]] =
        (v: M[Wra]) => v map unwrap

    /** If there is a galois from some value to the wrapped value, then there is also a galois from that value to the wrapper value */
    given isogalois[C](using gal: Galois[C, V]): Galois[C, W] =
        (v: C) => wrap(gal.inject(v))
