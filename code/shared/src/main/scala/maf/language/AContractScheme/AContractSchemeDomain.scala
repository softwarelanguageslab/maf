package maf.language.AContractScheme

import maf.modular.scheme.SchemeDomain
import maf.core.Address

trait AContractSchemeDomain extends SchemeDomain:
    implicit override lazy val lattice: AContractSchemeLattice[Value, Address]
