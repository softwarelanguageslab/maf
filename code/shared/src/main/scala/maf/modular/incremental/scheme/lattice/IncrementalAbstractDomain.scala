package maf.modular.incremental.scheme.lattice

import maf.core.{Address, Expression}
import maf.modular.AbstractDomain

trait IncrementalAbstractDomain[Expr <: Expression] extends AbstractDomain[Expr] {
  implicit override val lattice: IncrementalLattice[Value, Address]
  def addAddress(v: Value, source: Address): Value = addAddresses(v, Set(source))
  def addAddresses(v: Value, sources: Set[Address]): Value
  def removeAddresses(v: Value): Value
}
