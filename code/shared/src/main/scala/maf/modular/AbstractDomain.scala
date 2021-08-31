package maf.modular

import maf.core._

trait AbstractDomain[Expr <: Expression] extends ModAnalysis[Expr] {
  type Value <: Serializable
  implicit lazy val lattice: Lattice[Value]
}
