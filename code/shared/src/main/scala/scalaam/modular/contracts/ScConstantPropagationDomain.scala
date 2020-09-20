package scalaam.modular.contracts

import scalaam.core.Address
import scalaam.language.contracts.{ScCoProductLattice, ScLattice}

trait ScConstantPropagationDomain extends ScDomain {
  import scalaam.lattice.ConstantPropagation._

  val coProductLattice: ScCoProductLattice[I, B, Address] =
    new ScCoProductLattice[I, B, Address]()

  val lattice = coProductLattice.isScLattice
  type Value = coProductLattice.CoProductValue
}
