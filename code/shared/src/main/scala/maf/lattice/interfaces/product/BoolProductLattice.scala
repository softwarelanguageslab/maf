package maf.lattice.interfaces.product

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

class BoolProductLattice[B: BoolLattice, L: Lattice] extends ProductLattice[B, L] with BoolLattice[(B, L)] {
  def inject(b: Boolean): (B, L) = ProductLattice[B, L].injectLeft(BoolLattice[B].inject(b))
  def isTrue(b: (B, L)): Boolean = BoolLattice[B].isTrue(ProductLattice[B, L].left(b)) // TODO (maybe) use first projection
  def isFalse(b: (B, L)): Boolean = BoolLattice[B].isFalse(ProductLattice[B, L].left(b))
  def not(b: (B, L)): (B, L) = opLeft1(BoolLattice[B].not, b)
}
