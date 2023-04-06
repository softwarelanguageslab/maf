package maf.util.datastructures

import cats.kernel.Monoid

object MapOps:
    extension [K, V: Monoid](map: Map[K, V])
        def merge(other: Map[K, V]): Map[K, V] =
            (map.keys ++ other.keys).map((k) => k -> Monoid[V].combine(map.getOrElse(k, Monoid[V].empty), other.getOrElse(k, Monoid[V].empty))).toMap
