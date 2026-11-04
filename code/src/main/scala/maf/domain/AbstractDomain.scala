package maf.domain

import maf.domain.lattice.*
import maf.syntax.*

trait AbstractDomain[Expr <: Expression]:
    type Value <: Serializable
    implicit lazy val lattice: Lattice[Value]
    def domainName: String = "abstract domain"
