package maf.modular

import maf.core._

trait AbstractDomain[Expr <: Expression]:
    type Value <: Serializable
    implicit lazy val lattice: Lattice[Value]
    def domainName: String = "abstract domain"
