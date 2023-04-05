package maf.util

/** Keys stored in an HMap */
trait HMapKey:
    /** The value associated with the key */
    type Value

/** An abstract HMap value, either a single key-pair or a HMap containing multiple key-pairs
  */
sealed trait HMapValue[KT <: HMapKey]:
    /** Insert or replace a new key-value pair in the HMap */
    def put(key: KT, value: key.Value): HMapValue[KT]

    /** Retrieve the value associated with the key `key` */
    def get(key: KT): Option[key.Value]

    /** Retrieve all the keys stored in the HMap */
    def keys: Set[KT]

    /** Checks whether the given key is in the map */
    def contains[K <: KT](k: K): Boolean

    /** Check whether the set of key-value pairs in the HMap is empty */
    def isEmpty: Boolean

    /** Checks whether the HMap is a singleton */
    def isSingleton: Boolean

    /** Adjusts the value at the given `key` by calling function `f` */
    def adjustAt(key: KT)(f: Option[key.Value] => key.Value): HMapValue[KT] =
        put(key, f(get(key)))

/** A key-tagged key-value pair. The key determines the type of the value by associating the value with an abstract type member of the key
  *
  * @param key
  *   an instance of the key (usually there is only a single one) meaning that key.type == K
  * @param content
  *   the value of the pair, must type-correspond to the `Value` type member of the key
  * @tparam K
  *   the type of the key
  * @example
  *   case object
  */
class KeyPair[KT <: HMapKey](val key: KT, content: key.Value) extends HMapValue[KT]:
    def put(nkey: KT, nvalue: nkey.Value): HMapValue[KT] =
        HMap.empty
            .put(nkey, nvalue)
            .put(key, content)

    def isSingleton: Boolean = true

    /** Retrieve the value associated with the key `key` */
    def get(k: KT): Option[k.Value] =
        if key == k then Some(content.asInstanceOf[k.Value]) else None

    /** Retrieve all the keys stored in the HMap */
    def keys: Set[KT] = Set(key)

    def contains[K <: KT](k: K): Boolean = k == key

    /** Check whether the set of key-value pairs in the HMap is empty */
    def isEmpty: Boolean = false

    /** Alternative implementation of the `toString` method */
    override def toString(): String =
        s"{$key ↦ $content}"

/** An heterogenous map. Its key are represented by HMapKey such that `get` and `put` operations on the map can be implemented in a type safe manner.
  *
  * @tparam K
  *   the type of keys stored in the heterogenous map, must be at least a `HMapKey`
  */
case class HMap[KT <: HMapKey](
    contents: Map[KT, Any])
    extends HMapValue[KT]:
    def put(key: KT, value: key.Value): HMapValue[KT] =
        // if isEmpty then KeyPair(key, value)
        /*else*/ this.copy(contents = contents + (key -> value))

    def get(key: KT): Option[key.Value] =
        // NOTE: asInstanceOf is safe here since put only allows insertions of values with the corresponding key
        contents.get(key).map(_.asInstanceOf[key.Value])

    def keys: Set[KT] =
        contents.keys.toSet

    def contains[K <: KT](k: K): Boolean =
        contents.contains(k)

    def isSingleton: Boolean =
        contents.size == 1

    def isEmpty: Boolean = contents.isEmpty

    /** Alternative implementation of the `toString` method */
    override def toString(): String =
        contents.map { case (k, v) => s"$k ↦ $v" }.mkString("{", ",", "}")

object HMap:
    /** Returns an empty HMap */
    def empty[K <: HMapKey]: HMap[K] = HMap[K](Map())

/** An untyped version of the HMap, where the key information is thrown away */
type UntypedHMap = HMapValue[HMapKey]
