package maf.modular.incremental

import maf.core.{Address, Lattice}

/**
 * A lattice with annotations.
 * @tparam AL The actual lattice.
 * @tparam A  The type of annotations, in our case, addresses.
 */
trait IncrementalLattice[AL, A <: Address] extends Lattice[AL] {
  def addAddress(v: AL, address: A): AL = addAddresses(v, Set(address))
  def addAddresses(v: AL, addresses: Set[A]): AL
  def getAddresses(v: AL): Set[A]
  def clean(v: AL): AL
}
