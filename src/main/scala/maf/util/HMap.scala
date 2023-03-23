package maf.util

/** Keys stored in an HMap */
trait HMapKey:
  /** The value associated with the key */
  type Value

/** An heterogenous map. Its key are represented by HMapKey such that `get` and
  * `put` operations on the map can be implemented in a type safe manner.
  *
  * @tparam K
  *   the type of keys stored in the heterogenous map, must be at least a
  *   `HMapKey`
  */
case class HMap[KT <: HMapKey](
    contents: Map[KT, Any]
):
  /** Insert or replace a new key-value pair in the HMap */
  def put[K <: KT, V](key: K, value: V): HMap[KT] =
    this.copy(contents = contents + (key -> value))

  /** Retrieve the value associated with the key `key` */
  def get[K <: KT, V](key: K): Option[V] =
    // NOTE: asInstanceOf is safe here since put only allows insertions of values with the corresponding key
    contents.get(key).map(_.asInstanceOf[V])

  /** Retrieve all the keys stored in the HMap */
  def keys: Set[KT] =
    contents.keys.toSet

  /** Check whether the set of key-value pairs in the HMap is empty */
  def isEmpty: Boolean = contents.isEmpty

object HMap:
  /** Returns an empty HMap */
  def empty[K <: HMapKey]: HMap[K] = HMap[K](Map())
