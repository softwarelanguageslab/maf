package maf.util.worklist

import cats.Show

/** Generic typeclass for a work list. */
trait WorkList[WL[_]]:
    def empty[X]: WL[X]

    extension [X](wl: WL[X])
        /** Returns the first element of the work list. */
        def head: X

        /** Returns a work list containing all but the first element of the current work list. */
        def tail: WL[X]

        /** Returns a work list that contains all elements currently in this work list, as well as the new, given element. */
        def add(x: X): WL[X]

        /** Returns a work list that contains all elements currently in this work list, as well as the new, given element. */
        def +(x: X): WL[X] = add(x)

        /** Returns a work list that contains all elements currently in this work list, as well as the new, given elements. */
        def addAll(xs: Iterable[X]): WL[X]

        /** Returns a work list that contains all elements currently in this work list, as well as the new, given elements. */
        def ++(xs: Iterable[X]): WL[X] = addAll(xs)

        /** Returns a work list that contains all elements currently in this work list, except the specified element. */
        def -(x: X): WL[X]

        /** Returns a work list that results from mapping the given function over all elements in the current work list. */
        def map[Y](f: X => Y): WL[Y]

        /** Returns a work list that contains the elements in this work list, except the elements for which the given predicate returns 'false'. */
        def filter(f: X => Boolean): WL[X]

        /** Returns a work list only contains the elements in this work list for which the given predicate returns 'false'. */
        def filterNot(f: X => Boolean): WL[X]

        /** Predicate that returns true if the current work list is empty. */
        def isEmpty: Boolean

        /** Predicate that returns true if the current work list is non-empty. */
        def nonEmpty: Boolean

        /** Predicate that returns true if the work list contains the given node. */
        def contains(x: X): Boolean

        /** Returns a list containing all elements currently in this work list. */
        def toList: List[X]

        /** Returns a set containing all elements currently in this work list. */
        def toSet: Set[X]

object WorkList:
    def empty[WL[_], X](using wl: WorkList[WL]): WL[X] = wl.empty
    def apply[WL[_]: WorkList]: WorkList[WL] = summon

    given [WL[_]: WorkList, X]: Show[WL[X]] with
        /** Returns a string representation of the worklist. */
        def show(self: WL[X]): String = WorkList[WL].toList(self).mkString("{", ", ", "}")
