package maf.analysis.typeclasses

import maf.analysis.primitives.*
import maf.values.scheme.*
import maf.util.Address

/** Abstract domain with primitives that can be used in an semantics */
trait SchemeAnalysisDomain[V, Vec, Pai]:

    // shorthands
    type Prim = SchemePrimitive[V, Vec, Pai, Address]
    type Val = V

    /** The abstract domain for Scheme values */
    given domain: SchemeDomain[V, Vec, Pai]

    /** A set of primitives */
    given primitives: SchemeLatticePrimitives[V, Vec, Pai]

    /** Shorthand to access the Scheme lattice within in the abstract domain */
    given lattice: SchemeLattice[V, Vec, Pai] = domain.schemeLattice
