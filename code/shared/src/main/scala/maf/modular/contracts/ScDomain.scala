package maf.modular.contracts

import maf.core.Address
import maf.language.contracts.{ScExp, ScLattice}
import maf.modular.AbstractDomain
import maf.lattice.interfaces.BoolLattice

trait ScDomain extends AbstractDomain[ScExp] {
  implicit val lattice: ScLattice[Value, Address]
}
