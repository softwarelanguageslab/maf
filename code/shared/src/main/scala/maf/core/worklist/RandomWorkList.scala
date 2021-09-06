package maf.core.worklist

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
