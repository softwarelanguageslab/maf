package maf.values
package typeclasses

import maf.util.*

trait LatticeWithAddrs[L, A <: Address] extends Lattice[L]:
  def refs(x: L): Set[A]
