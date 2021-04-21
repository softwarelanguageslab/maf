package maf.lattice.instances.taint

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

  sealed trait T[Taint]
  case object Tainted extends T[Unit]
  case object Untainted extends T[Unit]

  class SimpleTaintLattice extends TaintLattice[Unit, T] {
    def show(v: T[Unit]): String = v.toString
    def bottom: T[Unit] = Untainted
    override def top: T[Unit] = Tainted

    def join(x: T[Unit], y: => T[Unit]): T[Unit] = (x, y) match {
      case (Untainted, Untainted) => Untainted
      case _                      => Tainted
    }

    def subsumes(x: T[Unit], y: => T[Unit]): Boolean = (x, y) match {
      case (_, Untainted)     => true
      case (Tainted, Tainted) => true
      case _                  => false
    }

    def eql[B: BoolLattice](x: T[Unit], y: T[Unit]): B = BoolLattice[B].inject(x == y)

    override def inject(t: Unit): T[Unit] = Tainted
  }
}
