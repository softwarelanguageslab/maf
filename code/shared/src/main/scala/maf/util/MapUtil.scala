package maf.util

import maf.core.Lattice

object MapUtil {

  /**
    * A map that contains values from a certain lattice.
    *
    * @param inner the original map
    */
  implicit class MapWithLatticeValues[K, V: Lattice](inner: Map[K, V]) {

    /**
      * Put the given value on the given key, instead of strongly updating
      * the value associated with the key (if the key is already present)
      * the original will be merged with the new value according to the lattice
      * of V.
      *
      * @param k the key to add to the map
      * @param v the value to associate with the given key
      */
    def weakPut(k: K, v: V): Map[K, V] = {
      val value = inner.get(k).map(old => Lattice[V].join(old, v)).getOrElse(v)
      inner + (k -> value)
    }
  }

}
