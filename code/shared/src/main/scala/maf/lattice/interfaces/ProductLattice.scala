package maf.lattice.interfaces

import maf.core.Lattice

class ProductLattice[L: Lattice, R: Lattice] extends Lattice[(L, R)] {

  def bottom: (L, R) = (Lattice[L].bottom, Lattice[R].bottom)

  def join(x: (L, R), y: => (L, R)): (L, R) = (Lattice[L].join(x._1, y._1), Lattice[R].join(x._2, y._2))

  def subsumes(x: (L, R), y: => (L, R)): Boolean = Lattice[L].subsumes(x._1, y._1) && Lattice[R].subsumes(x._2, y._2)

  def eql[B: BoolLattice](x: (L, R), y: (L, R)): B = BoolLattice[B].join(Lattice[L].eql(x._1, y._1), Lattice[R].eql(x._2, y._2))

  def show(v: (L, R)): String = s"(${Lattice[L].show(v._1)} × ${Lattice[R].show(v._2)})"
}
