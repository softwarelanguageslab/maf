package maf.lattice

import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice
import maf.util.Wrapper

object WrappedLattice {
  implicit def wrappedLattice[V: Lattice, F](implicit wrapper: Wrapper[V, F]): Lattice[F] =
    new Lattice[F] {

      /** A lattice has a bottom element */
      override def bottom: F = wrapper.wrap(Lattice[V].bottom)

      /** A lattice has a top element (might be undefined) */
      override def top: F = wrapper.wrap(Lattice[V].top)

      /** Elements of the lattice can be joined together */
      override def join(x: F, y: => F): F =
        wrapper.wrap(Lattice[V].join(wrapper.unwrap(x), wrapper.unwrap(y)))

      /** Subsumption between two elements can be checked */
      override def subsumes(x: F, y: => F): Boolean =
        Lattice[V].subsumes(wrapper.unwrap(x), wrapper.unwrap(y))

      /** Equality check, returning an abstract result */
      override def eql[B: BoolLattice](x: F, y: F): B =
        Lattice[V].eql(wrapper.unwrap(x), wrapper.unwrap(y))

      override def show(v: F): String =
        Lattice[V].show(wrapper.unwrap(v))
    }
}
