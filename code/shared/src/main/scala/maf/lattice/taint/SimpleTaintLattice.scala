package maf.lattice.taint

import maf.lattice.interfaces.BoolLattice
import maf.util.Show

object SimpleTaintLattice {

  sealed trait T

  case object Tainted extends T

  case object MaybeTainted extends T

  case object Untainted extends T

  class SimpleTaintLattice extends TaintLattice[T] {
    override def show(v: T): String = v.toString

    /** A lattice has a bottom element */
    override def bottom: T = Untainted

    /** A lattice has a top element (might be undefined) */
    override def top: T = Tainted

    /** Elements of the lattice can be joined together */
    override def join(x: T, y: => T): T = (x, y) match {
      case (Untainted, _) => y
      case (_, Untainted) => x
      case (Tainted, _)   => Tainted
      case (_, Tainted)   => Tainted
      case _              => MaybeTainted
    }

    /** Subsumption between two elements can be checked */
    override def subsumes(x: T, y: => T): Boolean = ???

    /** Equality check, returning an abstract result */
    override def eql[B: BoolLattice](x: T, y: T): B = ???
  }
}
