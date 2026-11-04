package maf.fixpoint

import scala.collection.immutable.Queue

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

object WorkList:
    def empty[X]: WorkList[X] = LIFOWorkList.empty

/** A worklist with deterministic breadth-first exploration order. */
case class FIFOWorkList[X](queue: Queue[X], set: Set[X]) extends WorkList[X]:
    def isEmpty: Boolean = queue.isEmpty
    def nonEmpty: Boolean = queue.nonEmpty
    def head: X = queue.head
    def tail: FIFOWorkList[X] = FIFOWorkList(queue.tail, set - queue.head)
    def add(x: X): FIFOWorkList[X] = if set.contains(x) then { this }
    else { FIFOWorkList(queue.enqueue(x), set + x) }
    def addAll(xs: Iterable[X]): FIFOWorkList[X] = xs.foldLeft(this)((acc, elm) => acc.add(elm))
    def map[Y](f: X => Y): FIFOWorkList[Y] = FIFOWorkList(queue.map(f))
    def toList: List[X] = queue.toList
    def toSet: Set[X] = set
    def filter(f: X => Boolean): FIFOWorkList[X] = FIFOWorkList(queue.filter(f), set.filter(f))
    def filterNot(f: X => Boolean): FIFOWorkList[X] = FIFOWorkList(queue.filterNot(f), set.filterNot(f))
    def contains(x: X): Boolean = set.contains(x)
    def -(x: X): FIFOWorkList[X] = FIFOWorkList(queue.filter(_ != x), set - x)

object FIFOWorkList:
    def empty[X]: FIFOWorkList[X] = FIFOWorkList(Queue[X](), Set[X]())
    def apply[X](xs: Iterable[X]): FIFOWorkList[X] = empty.addAll(xs)
    def apply[X](xs: X*): FIFOWorkList[X] = apply(xs)

/** A worklist with deterministic depth-first exploration order. */
case class LIFOWorkList[X](lst: List[X], set: Set[X]) extends WorkList[X]:
    def isEmpty: Boolean = lst.isEmpty
    def nonEmpty: Boolean = lst.nonEmpty
    def head: X = lst.head
    def tail: LIFOWorkList[X] = LIFOWorkList(lst.tail, set - lst.head)
    def add(x: X): LIFOWorkList[X] = if set.contains(x) then { this }
    else { LIFOWorkList(x :: lst, set + x) }
    def addAll(xs: Iterable[X]): LIFOWorkList[X] = xs.foldLeft(this)((acc, elm) => acc.add(elm))
    def map[Y](f: X => Y): LIFOWorkList[Y] = LIFOWorkList(lst.map(f).reverse)
    def toList: List[X] = lst
    def toSet: Set[X] = set
    def filter(f: X => Boolean): LIFOWorkList[X] = LIFOWorkList(lst.filter(f), set.filter(f))
    def filterNot(f: X => Boolean): LIFOWorkList[X] = LIFOWorkList(lst.filterNot(f), set.filterNot(f))
    def contains(x: X): Boolean = set.contains(x)
    def -(x: X): LIFOWorkList[X] = LIFOWorkList(lst.filter(_ != x), set - x)

object LIFOWorkList:
    def empty[X]: LIFOWorkList[X] = LIFOWorkList(List[X](), Set[X]())
    def apply[X](xs: Iterable[X]): LIFOWorkList[X] = empty.addAll(xs)
    def apply[X](xs: X*): LIFOWorkList[X] = apply(xs)

/** A worklist with an arbitrary exploration order. */
case class RandomWorkList[X](set: Set[X]) extends WorkList[X]:
    def isEmpty: Boolean = set.isEmpty
    def nonEmpty: Boolean = set.nonEmpty
    def head: X = set.head
    def tail: RandomWorkList[X] = RandomWorkList(set.tail)
    def add(x: X): RandomWorkList[X] = RandomWorkList(set + x)
    def addAll(xs: Iterable[X]): RandomWorkList[X] = xs.foldLeft(this)((acc, elm) => acc.add(elm))
    def map[Y](f: X => Y): RandomWorkList[Y] = RandomWorkList(set.map(f))
    def toList: List[X] = set.toList
    def toSet: Set[X] = set
    def filter(f: X => Boolean): RandomWorkList[X] = RandomWorkList(set.filter(f))
    def filterNot(f: X => Boolean): RandomWorkList[X] = RandomWorkList(set.filterNot(f))
    def contains(x: X): Boolean = set.contains(x)
    def -(x: X): RandomWorkList[X] = RandomWorkList(set - x)

object RandomWorkList:
    def empty[X]: RandomWorkList[X] = RandomWorkList(Set[X]())
    def apply[X](xs: Iterable[X]): RandomWorkList[X] = empty.addAll(xs)
    def apply[X](xs: X*): RandomWorkList[X] = apply(xs)
