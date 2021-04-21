package maf.lattice.interfaces

import maf.core.Lattice

trait ProductLattice[LE, L <: Lattice[LE], RE, R <: Lattice[RE]] extends Lattice[(LE, RE)] {
  val left: L
  val right: R

  def bottom: (LE, RE) = (left.bottom, right.bottom)

  def join(x: (LE, RE), y: => (LE, RE)): (LE, RE) = (left.join(x._1, y._1), right.join(x._2, y._2))

  def subsumes(x: (LE, RE), y: => (LE, RE)): Boolean = left.subsumes(x._1, y._1) && right.subsumes(x._2, y._2)

  def eql[B: BoolLattice](x: (LE, RE), y: (LE, RE)): B = BoolLattice[B].join(left.eql(x._1, y._1), right.eql(x._2, y._2))

  def show(v: (LE, RE)): String = s"(${left.show(v._1)} × ${right.show(v._2)})"
}
