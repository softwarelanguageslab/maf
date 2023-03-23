package maf.util

trait Key:
  type Value

  /** Pattern match the key against a key-value pair, matches if the pair
    * contains a corresponding key.
    */
  def unapply[K <: Key](v: KeyPair[K]): Option[Value] =
    v.getOption(this)

object Key:
  /** Auxilary trait to specify the key as type member */
  trait Aux[E] extends Key { type Value = E }

/** A key-tagged key-value pair. The key determines the type of the value by
  * associating the value with an abstract type member of the key
  *
  * @param key
  *   an instance of the key (usually there is only a single one) meaning that
  *   key.type == K
  * @param content
  *   the value of the pair, must type-correspond to the `Value` type member of
  *   the key
  * @tparam K
  *   the type of the key
  * @example
  *   case object
  */
class KeyPair[K <: Key](val key: K, content: key.Value):
  /** Returns the contents of the pair if the given key corresponds to the
    * stored key
    */
  def getOption[K <: Key](k: K): Option[k.Value] =
    // SAFETY: asInstanceOf is safe since `k` is the same key as the one associated with the pair's content
    if is(k, key) then Some(content.asInstanceOf[k.Value]) else None

  /** Checks whether the current contents correspond to key `k` */
  def is[K](k: K): Boolean = key == k
