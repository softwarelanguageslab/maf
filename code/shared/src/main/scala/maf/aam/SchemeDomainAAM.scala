package maf.aam

import maf.language.scheme.primitives.*
import maf.language.scheme.lattices.*
import maf.core.*

trait SchemeDomainAAM:
    type Value

    /** Type of primitive functions. */
    type Prim = SchemePrimitive[Value, Address]

    /** Contains the implementation of the primitives for the given abstract domain. */
    lazy val primitives: SchemePrimitives[Value, Address]

    /** Implementation of abstract values. */
    implicit lazy val lattice: SchemeLattice[Value, Address]
