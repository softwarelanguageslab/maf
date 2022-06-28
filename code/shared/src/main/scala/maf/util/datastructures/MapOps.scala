package maf.util.datastructures

import maf.core.Lattice

object MapOps:
    extension [K, V](map: Map[K, V])
        def useDefaultValue(v: V): MapWithDefault[K, V] =
            MapWithDefault.withDefaultValue(map, v)

    extension [K, V: Lattice](iterable: Iterable[(K, V)])
        def toMapJoined: Map[K, V] =
            iterable.foldLeft(Map().useDefaultValue(Lattice[V].bottom)) { case (m, (k, v)) =>
                m.update(k)(Lattice[V].join(_, v))
            }

    case class MapWithDefault[K, V] private (contents: Map[K, V]) extends Map[K, V]:
        // A MapWithDefault behaves the same as its contents
        export contents.*

        /** Alternative for updatedWith, since we can assume that every key has a value */
        def update[V1 >: V](key: K)(remappingFunction: V => V1): MapWithDefault[K, V1] =
            MapWithDefault(contents = contents + (key -> remappingFunction(contents(key))))

    object MapWithDefault:
        private def apply[K, V](contents: Map[K, V]): MapWithDefault[K, V] =
            // Apply is private such that instances of MapWithDefault can only be constructed correctly
            new MapWithDefault(contents)

        def withDefaultValue[K, V](contents: Map[K, V], v: V): MapWithDefault[K, V] =
            new MapWithDefault(contents.withDefaultValue(v))
