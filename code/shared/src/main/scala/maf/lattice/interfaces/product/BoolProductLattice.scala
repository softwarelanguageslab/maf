package maf.lattice.interfaces.product

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

class BoolProductLattice[B: BoolLattice, L: Lattice] extends ProductLattice[B, L] with BoolLattice[(B, L)] {
  def inject(b: Boolean): (B, L) = (BoolLattice[B].inject(b), Lattice[L].bottom)
  def isTrue(b: (B, L)): Boolean = BoolLattice[B].isTrue(b._1) // TODO (maybe) use first projection
  def isFalse(b: (B, L)): Boolean = BoolLattice[B].isFalse(b._1)
  def not(b: (B, L)): (B, L) = opLeft1(BoolLattice[B].not, b)
}
