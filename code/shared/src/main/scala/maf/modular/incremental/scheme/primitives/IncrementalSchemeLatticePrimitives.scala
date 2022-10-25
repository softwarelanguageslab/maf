package maf.modular.incremental.scheme.primitives

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives.SchemeLatticePrimitives
import maf.modular.incremental.scheme.lattice.IncrementalSchemeLattice
import maf.core.Monad.MonadSyntaxOps

class IncrementalSchemeLatticePrimitives[V, A <: Address](implicit override val schemeLattice: IncrementalSchemeLattice[V, A]) extends SchemeLatticePrimitives[V, A]:

    override def ifThenElse[M[_] : PrimM](cond: M[V])(thenBranch: => M[V])(elseBranch: => M[V]): M[V] =
        PrimM[M].flatMap(cond) { condv =>
            val t = PrimM[M].flatMap(PrimM[M].guard(lat.isTrue(condv))) { _ => thenBranch } //.map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv))) }
            val f = PrimM[M].flatMap(PrimM[M].guard(lat.isFalse(condv))) { _ => elseBranch } //.map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv))) }
            PrimM[M].mjoin(t, f).map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv)))
        }

end IncrementalSchemeLatticePrimitives