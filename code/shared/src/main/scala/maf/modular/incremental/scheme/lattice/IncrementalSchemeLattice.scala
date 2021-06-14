package maf.modular.incremental.scheme.lattice

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice

/** An annotated lattice for Scheme. */
trait IncrementalSchemeLattice[AL, A <: Address] extends IncrementalLattice[AL, A] with SchemeLattice[AL, A]
