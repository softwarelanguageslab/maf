package maf.lattice.interfaces

import maf.core._

trait LatticeWithAddrs[L, A <: Address] extends Lattice[L]:
    def refs(x: L): Set[A]
