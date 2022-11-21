package maf.modular.incremental.scheme.lattice

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice
import maf.util.datastructures.SmartUnion

/** An annotated lattice for Scheme. */
trait IncrementalSchemeLattice[AL, A <: Address] extends IncrementalLattice[AL, A] with SchemeLattice[AL, A]:

    override def and(x: AL, y: => AL): AL =
        val addr = SmartUnion.sunion(getAddresses(x), getAddresses(y))
        val res = super.and(x, y)
        addAddresses(res, addr)

    override def or(x: AL, y: => AL): AL =
        val addr = SmartUnion.sunion(getAddresses(x), getAddresses(y))
        val res = super.or(x, y)
        addAddresses(res, addr)

end IncrementalSchemeLattice
