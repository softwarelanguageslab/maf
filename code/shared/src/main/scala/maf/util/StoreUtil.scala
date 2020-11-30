package maf.util

import maf.core.Address
import maf.core.Store
import maf.util.benchmarks.Table

object StoreUtil {

  /**
    * An implicit class that provides a conversion from a map to a something
    * that implements the Store trait.
    *
    * @param map the map to base the store on
    */
  implicit class MapAsStore[A <: Address, V](val map: Map[A, V]) extends Store[A, V] {
    def lookup(a: A): Option[V]         = map.get(a)
    def extend(a: A, v: V): Store[A, V] = MapAsStore(map + (a -> v))
    def canEqual(that: Any): Boolean = that match {
      case m: MapAsStore[A, V] => map.canEqual(m.map)
      case _                   => false
    }

    def productArity: Int           = map.productArity
    def productElement(n: Int): Any = map.productElement(n)

    def asTable: Table[String] = {
      Table(
        map
          .zip(Iterator.from(0))
          .flatMap {
            case ((k, v), row) =>
              List(((row.toString, "Address"), k.toString), ((row.toString, "Value"), v.toString))
          }
          .toMap,
        None
      )
    }
  }
}
