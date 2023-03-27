package maf.values.domains

import maf.values.Lattice
import maf.util.types
import maf.util.types.{given, *}

class Join[P](using SummonForValues[Lattice, _, P]) {
  object LatticeJoin extends ZipWith[Lattice]:
    def apply[V](a: V, b: V)(using lat: Lattice[V]): V =
      lat.join(a, b)
    def default[V](using lat: Lattice[V]): V = lat.bottom

  def apply(a: SparseProduct[P], b: => SparseProduct[P]): SparseProduct[P] =
    a.zipWith(b)(LatticeJoin)

}

class Subsumes[P](using SummonForValues[Lattice, _, P]) {
  object LatticeSubsumes extends ForAll[Lattice]:
    def apply[V](a: V, b: V)(using lat: Lattice[V]): Boolean =
      lat.subsumes(a, b)
    def default[V](using lat: Lattice[V]): V = lat.bottom

  def apply(a: SparseProduct[P], b: => SparseProduct[P]): Boolean =
    a.forall(b)(LatticeSubsumes)

}

/** Split a product into its constituents */
trait Split[P]:
  type Val = SparseProduct[P]
  def apply(v: Val): Set[Val]

object Split:
  def apply[P: Split](v: SparseProduct[P]): Set[SparseProduct[P]] =
    summon[Split[P]](v)
  given splitSingle[K: KeyFor, V]: Split[(K ~> V)] with
    def apply(v: Val): Set[Val] =
      v.get(Key[K]) match
        case Some(v) => Set(SparseProduct.empty[(K ~> V)].put(Key[K], v))
        case _       => Set()

  given splitMulti[K: KeyFor, V, P](using
      nd: NoDuplicates[(K ~> V) :*: P],
      product: Product[P],
      splitFirst: Split[(K ~> V)],
      splitRest: Split[P]
  ): Split[(K ~> V) :*: P] with {
    def apply(v: Val): Set[Val] =
      val first = splitFirst(v.toFirst)
      val rest = splitRest(v.toRest)
      first
        .map(v =>
          // NOTE: we do `extend` here such that every value is of type (K ~> V) :*: P
          SparseProduct.empty[P].extend[K, V, P](Key[K], v.get(Key[K]).get)
        ) ++ rest.map(v =>
        // NOTE: extend is also used here to make the type of the product bigger
        // this is always safe since the bigger type will also contain the smaller one
        v.extend[K, V, P]
      )

  }
