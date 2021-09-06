package maf.core.worklist

/** Generic interface for a work list. */
trait WorkList[X]:

    /** Returns the first element of the work list. */
    def head: X

    /** Returns a work list containing all but the first element of the current work list. */
    def tail: WorkList[X]

    /** Returns a work list that contains all elements currently in this work list, as well as the new, given element. */
    def add(x: X): WorkList[X]

    /** Returns a work list that contains all elements currently in this work list, as well as the new, given element. */
    def +(x: X): WorkList[X] = add(x)

    /** Returns a work list that contains all elements currently in this work list, as well as the new, given elements. */
    def addAll(xs: Iterable[X]): WorkList[X]

    /** Returns a work list that contains all elements currently in this work list, as well as the new, given elements. */
    def ++(xs: Iterable[X]): WorkList[X] = addAll(xs)

    /** Returns a work list that contains all elements currently in this work list, except the specified element. */
    def -(x: X): WorkList[X]

    /** Returns a work list that results from mapping the given function over all elements in the current work list. */
    def map[Y](f: X => Y): WorkList[Y]

    /** Returns a work list that contains the elements in this work list, except the elements for which the given predicate returns 'false'. */
    def filter(f: X => Boolean): WorkList[X]

    /** Returns a work list only contains the elements in this work list for which the given predicate returns 'false'. */
    def filterNot(f: X => Boolean): WorkList[X]

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

    /** Returns a string representation of the worklist. */
    override def toString: String = toList.mkString("{", ", ", "}")

// Default implementation used in worklistMonoid.
object WorkList:
    def empty[X]: WorkList[X] = LIFOWorkList.empty
