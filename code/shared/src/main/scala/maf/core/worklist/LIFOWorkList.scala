package maf.core.worklist

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
