package maf.modular.incremental.scheme.lattice

import maf.core.{Address, Expression}
import maf.modular.AbstractDomain

/**
 * An abstract domain that can be annotated with addresses.
 * @tparam Expr
 *   The type of expressions of the language under analysis.
 */
trait IncrementalAbstractDomain[Expr <: Expression] extends AbstractDomain[Expr]:
    implicit override lazy val lattice: IncrementalLattice[Value, Address]
    override def domainName: String = "incremental abstract domain"
