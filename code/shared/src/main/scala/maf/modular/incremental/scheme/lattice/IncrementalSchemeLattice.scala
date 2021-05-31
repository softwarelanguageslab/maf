package maf.modular.incremental.scheme.lattice

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice

trait IncrementalSchemeLattice[V, A <: Address] extends IncrementalLattice[V, A] with SchemeLattice[V, A]
