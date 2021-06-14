package maf.modular.incremental.scheme.lattice

import maf.core.{Address, Expression}
import maf.modular.AbstractDomain

/**
 * An abstract domain that can be annotated with addresses.
 * @tparam Expr The type of expressions of the language under analysis.
 */
trait IncrementalAbstractDomain[Expr <: Expression] extends AbstractDomain[Expr] {
  implicit override val lattice: IncrementalLattice[Value, Address]
  def addAddress(v: Value, source: Address): Value = addAddresses(v, Set(source))
  def addAddresses(v: Value, sources: Set[Address]): Value
  def getAddresses(v: Value): Set[Address]
  def removeAddresses(v: Value): Value
}
