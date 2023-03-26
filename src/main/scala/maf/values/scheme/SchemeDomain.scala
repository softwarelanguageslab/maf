package maf.values
package scheme

import typeclasses.*

trait SchemeDomain:
  /* Simple Scheme values that may occur together in the store */
  type Val

  // Heap-allocated values
  type Str
  type Pai
  type Vec

  /** A lattice that operates on the non-heap allocated values */
  given lattice: SchemeLattice[Val]

  // Lattices for heap allocated values
  given stringLattice: StringLattice[Str, Val, Val, Val]
  given pairLattice: PairLattice[Pai]
  given vectorLattice: VectorLattice[Vec]

object SchemeDomain:
  trait Aux[V] extends SchemeDomain:
    type Val = V
