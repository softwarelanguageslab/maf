package maf.modular

import maf.core._

trait AbstractDomain[Expr <: Expression] extends ModAnalysis[Expr] {
    type Value
    implicit val lattice: Lattice[Value]
}
