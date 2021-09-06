package maf.lattice

import maf.lattice.ConcreteLattice

class BoundedLattice(val k: Int) extends ConcreteLattice:
    override def makeValues[X](contents: Set[X]): L[X] =
      if contents.size <= k then Values(contents)
      else Top
