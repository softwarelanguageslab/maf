package maf.modular.incremental.scheme.modconc

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice

trait IncrementalSchemeLattice[AL, A <: Address] extends SchemeLattice[AL, A] {
  def addAddress(v: AL, address: Address): AL = addAddresses(v, Set(address))
  def addAddresses(v: AL, addresses: Set[Address]): AL
  def clean(v: AL): AL
}
