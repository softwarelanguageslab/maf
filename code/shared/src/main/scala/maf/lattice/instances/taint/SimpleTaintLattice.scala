package maf.lattice.instances.taint

import maf.core.Address
import maf.lattice.interfaces.BoolLattice
import maf.lattice.interfaces.taint.TaintLattice

// format: off
/**
 * Implements a simple taint lattice.
 *
 *          Tainted
 *             |
 *         Untainted
 */
// format: on
object SimpleTaintLattice {

  type Taint = Unit
  sealed trait T[Taint]
  case object Tainted extends T[Taint]
  case object Untainted extends T[Taint]

  class SimpleTaintLattice extends TaintLattice[Taint, T] {
    def show(v: T[Taint]): String = v.toString
    def bottom: T[Taint] = Untainted
    override def top: T[Taint] = Tainted

    def join(x: T[Taint], y: => T[Taint]): T[Taint] = (x, y) match {
      case (Untainted, Untainted) => Untainted
      case _                      => Tainted
    }

    def subsumes(x: T[Taint], y: => T[Taint]): Boolean = (x, y) match {
      case (_, Untainted)     => true
      case (Tainted, Tainted) => true
      case _                  => false
    }

    def eql[B: BoolLattice](x: T[Taint], y: T[Taint]): B = BoolLattice[B].inject(x == y)

    override def inject(t: Unit): T[Taint] = Tainted
  }

  implicit val simpleTaintLattice: SimpleTaintLattice = new SimpleTaintLattice
}
