package maf.lattice.interfaces.product

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

// TODO maybe: make the product type opaque?
class ProductLattice[L: Lattice, R: Lattice] extends Lattice[(L, R)] {

  def bottom: (L, R) = (Lattice[L].bottom, Lattice[R].bottom)
  def join(x: (L, R), y: => (L, R)): (L, R) = (Lattice[L].join(x._1, y._1), Lattice[R].join(x._2, y._2))
  def subsumes(x: (L, R), y: => (L, R)): Boolean = Lattice[L].subsumes(x._1, y._1) && Lattice[R].subsumes(x._2, y._2)
  def eql[B: BoolLattice](x: (L, R), y: (L, R)): B = BoolLattice[B].join(Lattice[L].eql(x._1, y._1), Lattice[R].eql(x._2, y._2))
  def show(v: (L, R)): String = s"(${Lattice[L].show(v._1)} × ${Lattice[R].show(v._2)})"

  /** Creates a new value of a product lattice by binding two separate lattice values. */
  def compose(l: L, r: R): (L, R) = (l, r)

  /** First projection. */
  def left(x: (L, R)): L = x._1

  /** Second projection. */
  def right(x: (L, R)): R = x._2

  /** First projection of join. */
  def joinLeft(x: (L, R), y: => (L, R)): L = Lattice[L].join(x._1, y._1)

  /** Second projection of join. */
  def joinRight(x: (L, R), y: => (L, R)): R = Lattice[R].join(x._2, y._2)

  /** Returns an element of this lattice consisting of the operation applied to the first projection and the second projection. */
  def opLeft1(op: L => L, x: (L, R)): (L, R) = (op(x._1), x._2)

  /** Returns an element of this lattice consisting of the operation applied to the first projections and the join of the second projections. */
  def opLeft2(
      op: (L, L) => L,
      x: (L, R),
      y: (L, R)
    ): (L, R) = (op(x._1, y._1), Lattice[R].join(x._2, y._2))

  /** Returns an element of this lattice consisting of the first projection and the operation applied to the second projection. */
  def opRight1(op: R => R, x: (L, R)): (L, R) = (x._1, op(x._2))

  /** Returns an element of this lattice consisting of the join of the first projections and the operation applied to the second projections. */
  def opRight2(
      op: (R, R) => R,
      x: (L, R),
      y: (L, R)
    ): (L, R) = (Lattice[L].join(x._1, y._1), op(x._2, y._2))
}

object ProductLattice {
  def apply[L, R](implicit p: ProductLattice[L, R]): ProductLattice[L, R] = p
}
