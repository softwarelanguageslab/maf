package maf.values.domains

import maf.values.Lattice
import maf.util.types
import maf.util.types.{given, *}

/** A polymorphic function `Join` that operates on a labeled product
  *
  * @tparam P
  *   the type of labeled product
  */
trait Join[P]:
  type Val = SparseProduct[P]
  def join(a: Val, b: => Val): Val

object Join {
  def apply[P](using
      mjoin: Join[P]
  )(a: SparseProduct[P], b: => SparseProduct[P]): SparseProduct[P] =
    mjoin.join(a, b)

  given joinSingle[K, V](using
      keyFor: KeyFor[K],
      lat: Lattice[V]
  ): Join[(K ~> V)] with
    def join(
        a: SparseProduct[(K ~> V)],
        b: => SparseProduct[(K ~> V)]
    ): SparseProduct[(K ~> V)] =
      val (v1, v2) = (a.get(keyFor.key), b.get(keyFor.key))
      if v1.isEmpty && v2.isEmpty then a
      else
        a.put(
          keyFor.key,
          lat.join(
            a.get(keyFor.key).getOrElse(lat.bottom),
            b.get(keyFor.key).getOrElse(lat.bottom)
          )
        )

  given joinMulti[K: KeyFor, V: Lattice, P](using
      joinM: Join[(K ~> V)],
      joinRest: Join[P],
      noDup: NoDuplicates[(K ~> V) :*: P]
  ): Join[(K ~> V) :*: P] with {

    def join(a: Val, b: => Val): Val =
      val rest = joinRest.join(a.toRest, b.toRest)
      val current: SparseProduct[(K ~> V)] = joinM.join(a.toFirst, b.toFirst)
      val key = summon[KeyFor[K]].key
      current.get(key) match
        case Some(v) => rest.extend[K, V, P](key, v)
        case None    => rest.extend[K, V, P]

  }
}

trait Subsumes[P]:
  type Val = SparseProduct[P]
  def apply(a: Val, b: => Val): Boolean

object Subsumes:
  def apply[P: Subsumes](x: SparseProduct[P], y: => SparseProduct[P]) =
    summon[Subsumes[P]](x, y)

  given subsumesSingle[K: KeyFor, V](using lat: Lattice[V]): Subsumes[(K ~> V)]
    with
    def apply(a: Val, b: => Val): Boolean =
      lat.subsumes(
        a.get(Key[K]).getOrElse(lat.bottom),
        b.get(Key[K]).getOrElse(lat.bottom)
      )

  given subsumesMulti[K, V, P: Product](using
      subsumesFirst: Subsumes[(K ~> V)],
      subsumesRest: Subsumes[P]
  ): Subsumes[(K ~> V) :*: P] with
    def apply(a: Val, b: => Val): Boolean =
      subsumesFirst(a.toFirst, b.toFirst) && subsumesRest(a.toRest, b.toRest)
