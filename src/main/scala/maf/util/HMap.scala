package maf.util

/** Instances of this trait represent evidence that `K` may be mapped to `V` in
  * the heterogenous map.
  *
  * @tparam K
  *   the type of the key in the heterogenous map
  * @tparam V
  *   the type of the value in the heteregonous map
  * @note
  *   both type parameters a contravariant such that evidence of a mapping from
  *   `Any` -> `Any` is also evidence of a mapping from `K` -> `V` for any `K`
  *   and `V`.
  */
trait Mapping[-K, -V]

object Mapping:
  /** Auto-implementation of the `Mapping` trait. Provides evidence that K is
    * allowed to be mapped to V if in context
    */
  def derived[K, V] = new Mapping[K, V] {}

/** An heterogenous map. Its key are represented by HMapKey such that `get` and
  * `put` operations on the map can be implemented in a type safe manner.
  *
  * @tparam K
  *   the type of keys stored in the heterogenous map, must be at least a
  *   `HMapKey`
  * @note
  *   HMap keeps track of **evidence** that certain keys are allowed to be
  *   mapped to certain values. This is needed for operations that retrieve all
  *   the keys from the map, perform operations on their values, and put the
  *   keys with the updated values back into the map.
  */
case class HMap[KT](
    contents: Map[KT, Any],
    evidence: Map[KT, Mapping[Any, Any]]
):
  /** Insert or replace a new key-value pair in the HMap */
  def put[K <: KT, V](key: K, value: V)(using Mapping[K, V]): HMap[KT] =
    this.copy(contents = contents + (key -> value))

  /** Retrieve the value associated with the key `key` */
  def get[K <: KT, V](key: K)(using Mapping[K, V]): Option[V] =
    // NOTE: asInstanceOf is safe here since put only allows insertions of values with the corresponding key
    contents.get(key).map(_.asInstanceOf[V])

  /** Retrieve all the keys stored in the HMap */
  def keysWithEvidence: Set[(KT, Mapping[Any, Any])] =
    contents.keys.toSet.map(key => (key, evidence(key)))

  /** Check whether the set of key-value pairs in the HMap is empty */
  def isEmpty: Boolean = contents.isEmpty

object HMap:
  /** Returns an empty HMap */
  def empty[K]: HMap[K] = HMap(Map(), Map())
