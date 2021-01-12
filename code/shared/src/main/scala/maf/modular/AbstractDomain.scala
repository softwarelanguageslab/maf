package maf.modular

import maf.core._

trait AbstractDomain[Expr <: Expression] extends ModAnalysis[Expr] {
  type Value <: Serializable
  implicit val lattice: Lattice[Value]
}
