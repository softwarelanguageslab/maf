package maf.modular.incremental.scheme.primitives

import maf.core.Address
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives.SchemeLatticePrimitives
import maf.modular.incremental.scheme.lattice.IncrementalSchemeLattice
import maf.core.Monad.MonadSyntaxOps

class IncrementalSchemeLatticePrimitives[V, A <: Address](implicit override val schemeLattice: IncrementalSchemeLattice[V, A]) extends SchemeLatticePrimitives[V, A]:

    override def ifThenElse[M[_] : PrimM](cond: M[V])(thenBranch: => M[V])(elseBranch: => M[V]): M[V] =
        // Add the annotations from the condition to the result.
        // Needed to pass the annotations through the primitive functions (without modifying the `call` functions).
        PrimM[M].flatMap(cond) { condv =>
            val t = PrimM[M].flatMap(PrimM[M].guard(lat.isTrue(condv))) { _ => thenBranch } //.map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv))) }
            val f = PrimM[M].flatMap(PrimM[M].guard(lat.isFalse(condv))) { _ => elseBranch } //.map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv))) }
            PrimM[M].mjoin(t, f).map(v => schemeLattice.addAddresses(v, schemeLattice.getAddresses(condv))) // Don't just do super here as the branches are lazy.
        }

    /** Dereferences a pointer x (which may point to multiple addresses) and applies a function to its value, joining everything together */
    override def dereferencePointer[M[_] : PrimM, X: Lattice](x: V)(f: (A, V) => M[X]): M[X] =
        // Add the annotations from the pointer to the value obtained after dereferencing.
        // Needed because this would otherwise interrupt the found flows; the value of the pointer decides on the value after dereferencing.
        // Thus, when detecting cycles, no flow would be found although it is needed ~ implicit flows. Remark: Is this true? TODO: check this!
        super.dereferencePointer(x).map(v => schemeLattice.addAddresses(v, x))

end IncrementalSchemeLatticePrimitives