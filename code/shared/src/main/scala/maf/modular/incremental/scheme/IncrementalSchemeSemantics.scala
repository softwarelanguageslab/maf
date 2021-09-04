package maf.modular.incremental.scheme

import maf.language.scheme.*
import maf.modular.incremental.IncrementalModAnalysis
import maf.modular.incremental.scheme.lattice.IncrementalSchemeDomain

// We need to mix-in IncrementalSchemeDomain to be able to use the introduced operations in the semantics.
trait IncrementalSchemeSemantics extends IncrementalModAnalysis[SchemeExp] with IncrementalSchemeDomain
