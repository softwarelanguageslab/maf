package maf.language.scheme.lattices

import maf.language.scheme.lattices.SchemeLattice
import maf.core.Address

trait SchemeDomainConversion[A, B](
    using latA: SchemeLattice[A, Address]
  )(using latB: SchemeLattice[B, Address]
  )(using conv: SchemeValueConversion[latA.Value, latB.Value]):
    def convertValue(valA: latA.Value): latB.Value =
        conv.convert(valA)

trait SchemeValueConversion[A, B]:
    def convert(a: A): B
