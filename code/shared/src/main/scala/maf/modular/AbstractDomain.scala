package maf.modular

import maf.core._

trait AbstractDomain[Expr <: Expression]:
    type Value <: Serializable
    implicit lazy val lattice: Lattice[Value]
    def domainName: String = "abstract domain"

/**
 * An abstract domain that can be annotated with addresses.
 * @tparam Expr
 *   The type of expressions of the language under analysis.
 */
trait AnnotatedAbstractDomain[Expr <: Expression] extends AbstractDomain[Expr]:
    implicit override lazy val lattice: AnnotatedLattice[Value, Address]
    override def domainName: String = "annotated abstract domain"