package maf.domain.lattice

import maf.domain.*

trait LatticeWithAddrs[L, A <: Address] extends Lattice[L]:
    def refs(x: L): Set[A]
