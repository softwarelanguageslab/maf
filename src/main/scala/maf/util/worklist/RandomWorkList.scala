package maf.util.worklist

/** A worklist with an arbitrary exploration order. */
case class RandomWorkList[X](set: Set[X])

object RandomWorkList:
    def apply[X](xs: Iterable[X]): RandomWorkList[X] = instance.empty.addAll(xs)
    def apply[X](xs: X*): RandomWorkList[X] = apply(xs)

    given instance: WorkList[RandomWorkList] with
        def empty[X]: RandomWorkList[X] = RandomWorkList(Set[X]())
        extension [X](self: RandomWorkList[X])
            def isEmpty: Boolean = self.set.isEmpty
            def nonEmpty: Boolean = self.set.nonEmpty
            def head: X = self.set.head
            def tail: RandomWorkList[X] = RandomWorkList(self.set.tail)
            def add(x: X): RandomWorkList[X] = RandomWorkList(self.set + x)
            def addAll(xs: Iterable[X]): RandomWorkList[X] = xs.foldLeft(self)((acc, elm) => acc.add(elm))
            def map[Y](f: X => Y): RandomWorkList[Y] = RandomWorkList(self.set.map(f))
            def toList: List[X] = self.set.toList
            def toSet: Set[X] = self.set
            def filter(f: X => Boolean): RandomWorkList[X] = RandomWorkList(self.set.filter(f))
            def filterNot(f: X => Boolean): RandomWorkList[X] = RandomWorkList(self.set.filterNot(f))
            def contains(x: X): Boolean = self.set.contains(x)
            def -(x: X): RandomWorkList[X] = RandomWorkList(self.set - x)
