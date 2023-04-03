package maf.util
package types

import scala.annotation.implicitNotFound

/** A typelevel constructor for a product value, takes a left and a right value
  * that should be part of the product
  */
trait :*:[L, R]

/** A tuple for tagging the elements in the product value */
trait ~>[K, V]

//
// Keys
//

/** Return a key in the value domain for type K */
trait KeyFor[K]:
    def key: K

/** A key that is implicitly available for some K */
trait Key[K]:
    outer: K =>

    given KeyFor[K] with
        def key: K = outer

    def unapply[P, V](v: SparseProduct[P])(using KeyValueIn[K, V, P]): Option[V] =
        v.get(summon[KeyFor[K]].key)

object Key:
    def apply[K](using getKey: KeyFor[K]): K = getKey.key

//
// Membership
//

/** Predicate that is satisfied if the product value contains a pair with the
  * given key and value type
  */
@implicitNotFound("Key ${K} is not in product ${P}")
trait KeyValueIn[K, V, P]

/** Base case */
given [K, V]: KeyValueIn[K, V, (K ~> V)] with {}

/** If the key is in front of the product than it is in the product */
given [K, V, P]: KeyValueIn[K, V, (K ~> V) :*: P] with {}

/** If the key is not in front of the product, then it must be in its remainder
  */
given keyInRemainder[K1, K2, V, R](
    using KeyValueIn[K1, V, R]
): KeyValueIn[K1, V, K2 :*: R] with {}

/** Predicate that is satisfied if the product vlaue contains a pair with the
  * given key
  */
trait KeyIn[K, P]
given [K, V]: KeyIn[K, (K ~> V)] with {}
given [K, V, P]: KeyIn[K, (K ~> V) :*: P] with {}
given [K1, K2, V, P](using KeyIn[K1, P]): KeyIn[K2, P] with {}

//
// Duplication tests
//

/** Predicate that is satisfied if the product does not contain any duplicate
  * keys
  */
trait NoDuplicates[P]
object NoDuplicates:
    /** Entry */
    given [P](using NoDuplicatesAux[P, HNil]): NoDuplicates[P] with {}

/** Auxilary predicate to iteratively check that the product does not contain
  * any duplicate keys
  */
trait NoDuplicatesAux[P, L <: HList]

object NoDuplicatesAux:

    /** End of the product */
    given [K, V, L <: HList](
        using ContainsNot[K, L]
    ): NoDuplicatesAux[(K ~> V), L] with {}

    /** The first one should not be already in the list of seen elements */
    given [K, V, R, L <: HList](
        using NoDuplicatesAux[R, K :: L],
        ContainsNot[K, L]
    ): NoDuplicatesAux[(K ~> V) :*: R, L] with {}

/** A well-formed labeled product type should not contain any duplicates */
trait Product[P]
given [P](using NoDuplicates[P]): Product[P] with {}

//
// Sparse (HMap) interpretation
//
sealed trait AbstractSparseProductKey extends HMapKey:
    type Key
    val k: Key

case class SparseProductKey[K, V](val k: K) extends AbstractSparseProductKey:
    type Value = V
    type Key = K
    override def toString(): String = k.toString()

class SparseProduct[P: Product] private (
    private val map: HMapValue[AbstractSparseProductKey]) {
    type Content = P
    type Self[P] = SparseProduct[P]

    /** Retrieve a value corresponding to the given key from the product */
    def get[K, V](key: K)(using KeyValueIn[K, V, P]): Option[V] =
        map.get(SparseProductKey(key))

    /** Change or add a value on an existing key in the product */
    def put[K, V](key: K, vlu: V)(using KeyValueIn[K, V, P]): Self[P] =
        SparseProduct(map.put(SparseProductKey(key), vlu))

    /** Extend the product with the given key and value */
    def extend[K, V, P](
        key: K,
        vlu: V
      )(using Product[(K ~> V) :*: P]
      ): Self[(K ~> V) :*: P] =
        SparseProduct(map.put(SparseProductKey(key), vlu))

    /** Extend the product type contained within this map, but do not extend the
      * contents itself
      */
    def extend[K, V, P](
        using NoDuplicates[(K ~> V) :*: P]
      ): Self[(K ~> V) :*: P] = this.asInstanceOf[Self[(K ~> V) :*: P]]

    /** Checks whether the key is currently in the product, fails at
      * compile-time if the type of the product does not contain the key
      */
    def contains[K: KeyFor](using KeyIn[K, P]): Boolean =
        map.contains(SparseProductKey(Key[K]))

    /** Checks whether the HMap is a singleton */
    def isSingleton: Boolean = map.isSingleton

    def zipWith[F[_], IP](
        using
        getInstances: SummonForValues[F, IP, P]
      )(b: => SparseProduct[P]
      )(zipper: ZipWith[F]
      ): SparseProduct[P] =
        val instances = getInstances.instances
        SparseProduct(
          (map.keys ++ b.map.keys)
              .foldLeft(HMap.empty: HMapValue[AbstractSparseProductKey])(
                (map, key) =>
                    val Fval =
                        instances.get[key.Key, F[key.Value]](key.k)(using null)
                    map.put(
                      key,
                      zipper(
                        map.get(key).getOrElse(zipper.default(using Fval)),
                        b.map.get(key).getOrElse(zipper.default(using Fval))
                      )(using Fval)
                    )
              )
        )

    def forall[F[_], IP](
        b: SparseProduct[P]
      )(f: ForAll[F]
      )(using
        getInstances: SummonForValues[F, IP, P]
      ): Boolean =
        val instances = getInstances.instances
        (map.keys ++ b.map.keys).forall((key) =>
            val Fval =
                instances.get[key.Key, F[key.Value]](key.k)(using null)

            f(
              map.get(key).getOrElse(f.default(using Fval)),
              b.map.get(key).getOrElse(f.default(using Fval))
            )(
              using Fval
            )
        )

    override def toString(): String =
        map.toString()
}

trait ZipWith[F[_]]:
    def apply[V](a: V, b: V)(using F[V]): V
    def default[V](using F[V]): V

trait ForAll[F[_]]:
    def apply[V](a: V, b: V)(using F[V]): Boolean
    def default[V](using F[V]): V

object SparseProduct:
    def empty[P: Product]: SparseProduct[P] = SparseProduct(HMap.empty)

//
// Utilities for recursion
//

/** Predicate that is satisfied if `M` is the first element of `P` */
trait First[P, M]:
    def first(p: P): M
object First:
    given [K, V, P]: First[SparseProduct[(K ~> V) :*: P], SparseProduct[(K ~> V)]]
        with {
        def first(p: SparseProduct[(K ~> V) :*: P]): SparseProduct[(K ~> V)] =
            p.asInstanceOf[SparseProduct[(K ~> V)]]
    }

/** Predicate that is satisfied if `M` is everything but the first element of
  * `P`
  */
trait Rest[P, M]:
    def rest(p: P): M
object Rest:
    given [K, V, P]: Rest[SparseProduct[(K ~> V) :*: P], SparseProduct[P]]
        with {
        def rest(p: SparseProduct[K ~> V :*: P]): SparseProduct[P] =
            p.asInstanceOf[SparseProduct[P]]
    }

extension [P](p: P)
    def toFirst[M](using First[P, M]): M = summon[First[P, M]].first(p)
    def toRest[M](using Rest[P, M]): M = summon[Rest[P, M]].rest(p)

//
// A tagged tuple implementation
//

/** A (non-sparse) version of a product, ensures that all keys in the product
  * are available
  */
private class TaggedTuple[P: Product] private (
    val map: HMapValue[AbstractSparseProductKey]) {

    /** Retrieve a particular key from the tuple, ensures that the value is in
      * the tuple at compile-time
      */
    def get[K, V](key: K)(using KeyValueIn[K, V, P]): V =
        map.get(SparseProductKey(key)).get

    /** Extend the tuple with an additional key-value pair */
    def extend[K, V](
        using NoDuplicates[(K ~> V) :*: P]
      )(key: K,
        vlu: V
      ): TaggedTuple[(K ~> V) :*: P] =
        TaggedTuple(map.put(SparseProductKey(key), vlu))
}

object TaggedTuple:
    def initial[K, V](k: K, v: V): TaggedTuple[(K ~> V)] =
        TaggedTuple(HMap.empty.put(SparseProductKey(k), v))

//
// Utilities
//

/* Summon instances of a particular type for all the values in the product */
trait SummonForValues[F[_], IP, P]:
    def instances: TaggedTuple[IP]

object SummonForValues:
    given [F[_], K, V](using
        instance: F[V],
        key: KeyFor[K]
    ): SummonForValues[F, (K ~> F[V]), (K ~> V)] with {

        def instances: TaggedTuple[K ~> F[V]] =
            TaggedTuple.initial[K, F[V]](key.key, instance)
    }

    given [F[_], K, V, P, IP](using
        instance: F[V],
        summonForAll: SummonForValues[F, IP, P],
        noDup: NoDuplicates[(K ~> F[V]) :*: IP],
        key: KeyFor[K]
    ): SummonForValues[F, (K ~> F[V]) :*: IP, (K ~> V) :*: P] with {
        def instances: TaggedTuple[(K ~> F[V]) :*: IP] =
            summonForAll.instances
                .extend[K, F[V]](key.key, instance)
    }
