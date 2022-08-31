package maf.language.AScheme

import maf.modular.scheme.SchemeDomain
import maf.core.*

trait ASchemeDomain extends SchemeDomain:
    implicit override lazy val lattice: ASchemeLattice[Value, Address]
