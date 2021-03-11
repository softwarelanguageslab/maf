package maf.lattice

import maf.core.{Lattice, LatticeTopUndefined}
import maf.lattice.interfaces.BoolLattice

object MapLattice {
  implicit def mapLattice[K, V: Lattice]: Lattice[Map[K, V]] = new Lattice[Map[K, V]] {

    /** A lattice has a bottom element */
    override def bottom: Map[K, V] = Map()

    /** A lattice has a top element (might be undefined) */
    override def top: Map[K, V] = throw LatticeTopUndefined

    /** Elements of the lattice can be joined together */
    override def join(x: Map[K, V], y: => Map[K, V]): Map[K, V] =
      (x.keys ++ y.keys)
        .map(
          k =>
            k -> Lattice[V].join(
              x.withDefaultValue(Lattice[V].bottom)(k),
              y.withDefaultValue(Lattice[V].bottom)(k)
            )
        )
        .toMap

    /** Subsumption between two elements can be checked */
    override def subsumes(x: Map[K, V], y: => Map[K, V]): Boolean =
      x.forall {
        case (key, value) =>
          Lattice[V].subsumes(value, y.withDefaultValue(Lattice[V].bottom)(key))
      }

    /** Equality check, returning an abstract result */
    override def eql[B: BoolLattice](x: Map[K, V], y: Map[K, V]): B =
      x.foldLeft(Lattice[B].bottom)(
        (acc, current) =>
          Lattice[B].join(acc, current match {
            case (key, value) =>
              Lattice[V].eql(value, y.withDefaultValue(Lattice[V].bottom)(key))
          })
      )

    override def show(v: Map[K, V]): String = ""
  }
}
