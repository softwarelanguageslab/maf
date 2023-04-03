package maf.analysis

import maf.values.scheme.SchemeLattice
import maf.analysis.primitives.SchemeLatticePrimitives

trait SchemeDomain[V]:
  def primitives: SchemeLatticePrimitives[V]
  def lattice: SchemeLattice[V]

object SchemeDomain:
  def apply[V: SchemeDomain]: SchemeDomain[V] = summon
