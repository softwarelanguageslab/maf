package maf.util

import maf.core.{Lattice, Monad}

/**
 * A tagged set behaves just like a set, but each value in the set carries an additional optional tag
 *
 * @tparam X
 *   the type of the untagged value
 * @tparam T
 *   the type of the tagged value
 */
case class TaggedSet[T, X](vs: Set[(Option[T], X)]):
    def ++(other: TaggedSet[T, X]): TaggedSet[T, X] =
      TaggedSet(vs ++ other.vs)

    /** Returns the set of tags embedded in the tagged set (unordered), the connection with the tagged values is therefore lost. */
    def tags: Set[T] = vs.flatMap(_._1)

    def merge(using Lattice[X]): X =
      vs.map(_._2).foldLeft(Lattice[X].bottom)((acc, el) => Lattice[X].join(acc, el))

object TaggedSet:
    /** A monad instance for the tagged set that is fixed in the type of the tag */
    given taggedSetMonad[T]: Monad[[X] =>> TaggedSet[T, X]] with
        type M[X] = TaggedSet[T, X]
        // A unit returns the an untagged value
        def unit[X](x: X): M[X] = TaggedSet(Set((None, x)))
        // Map preserves the tag of the original value
        def map[X, Y](m: M[X])(f: X => Y): M[Y] = TaggedSet(m.vs.map { case (tag, v) =>
          (tag, f(v))
        })
        // Flatmap use the tag of the resulting tagged set
        def flatMap[X, Y](m: M[X])(f: X => M[Y]): M[Y] = TaggedSet(m.vs.flatMap { case (tag, v) =>
          f(v).vs
        })

    def empty[T, X]: TaggedSet[T, X] = TaggedSet(Set())

    /** Extract the tag from the values in the set and return a set of pairs of values with their tags */
    def extract[T, X](s: TaggedSet[T, X]): TaggedSet[T, (Option[T], X)] = TaggedSet(s.vs.map((None, _)))

    /** Replace the tag in the given tagged set with the given tag */
    def tag[T, X](tag: T, v: X): TaggedSet[T, X] =
      TaggedSet(Set((Some(tag), v)))

    def flatten[T, X](sets: Set[TaggedSet[T, X]]): TaggedSet[T, X] =
      sets.foldLeft(TaggedSet.empty)((acc, el) => TaggedSet(acc.vs ++ el.vs))
