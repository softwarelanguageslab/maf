package maf.lattice.interfaces.product

import maf.core.Lattice
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, StringLattice}

class CharProductLattice[C: CharLattice, L: Lattice] extends ProductLattice[C, L] with CharLattice[(C, L)] {
  def inject(c: Char): (C, L) = ProductLattice[C, L].injectLeft(CharLattice[C].inject(c))
  def downCase(c: (C, L)): (C, L) = opLeft1(CharLattice[C].downCase, c)
  def upCase(c: (C, L)): (C, L) = opLeft1(CharLattice[C].upCase, c)
  def toInt[I: IntLattice](c: (C, L)): I = ???
  def toString[S: StringLattice](c: (C, L)): S = ???
  def isLower[B: BoolLattice](c: (C, L)): B = ???
  def isUpper[B: BoolLattice](c: (C, L)): B = ???
  def charEq[B: BoolLattice](c1: (C, L), c2: (C, L)): B = ???
  def charLt[B: BoolLattice](c1: (C, L), c2: (C, L)): B = ???
  def charEqCI[B: BoolLattice](c1: (C, L), c2: (C, L)): B = ???
  def charLtCI[B: BoolLattice](c1: (C, L), c2: (C, L)): B = ???
}
