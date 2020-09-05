package scalaam.modular.contracts

import scalaam.core.Address
import scalaam.language.contracts.{ScExp, ScLattice}
import scalaam.modular.AbstractDomain

trait ScDomain extends AbstractDomain[ScExp] {
  implicit val lattice: ScLattice[Value, Address]
}
