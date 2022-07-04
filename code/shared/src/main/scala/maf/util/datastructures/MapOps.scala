package maf.util.datastructures

import maf.core.Lattice
import maf.util.Monoid

object MapOps:
    extension [K, V](map: Map[K, V])
        def useDefaultValue(v: V): MapWithDefault[K, V] =
            MapWithDefault.withDefaultValue(map, v)

    extension [K, V: Lattice](iterable: Iterable[(K, V)])
        def toMapJoined: Map[K, V] =
            iterable.foldLeft(Map().useDefaultValue(Lattice[V].bottom)) { case (m, (k, v)) =>
                m.update(k)(Lattice[V].join(_, v))
            }

    extension [K, V: Monoid, M <: Map[K, V]](m: Iterable[M])
        def toMapAppended: Map[K, V] =
            m.reduce((m, c) =>
                m ++ (m.map { case (k, v1) =>
                    m.get(k) match
                        case Some(v2) => k -> Monoid[V].append(v2, v1)
                        case None     => k -> v1
                })
            )

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
