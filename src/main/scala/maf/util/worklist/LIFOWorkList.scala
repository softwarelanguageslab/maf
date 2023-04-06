package maf.util.worklist

/** A worklist with deterministic depth-first exploration order. */
case class LIFOWorkList[X](lst: List[X], set: Set[X])

object LIFOWorkList:

    def apply[X](xs: Iterable[X]): LIFOWorkList[X] = instance.empty.addAll(xs)
    def apply[X](xs: X*): LIFOWorkList[X] = apply(xs)

    given instance: WorkList[LIFOWorkList] with
        def empty[X]: LIFOWorkList[X] = LIFOWorkList(List[X](), Set[X]())
        extension [X](wl: LIFOWorkList[X])
            def isEmpty: Boolean = wl.lst.isEmpty
            def nonEmpty: Boolean = wl.lst.nonEmpty
            def head: X = wl.lst.head
            def tail: LIFOWorkList[X] = LIFOWorkList(wl.lst.tail, wl.set - wl.lst.head)
            def add(x: X): LIFOWorkList[X] = if wl.set.contains(x) then { wl }
            else { LIFOWorkList(x :: wl.lst, wl.set + x) }
            def addAll(xs: Iterable[X]): LIFOWorkList[X] = xs.foldLeft(wl)((acc, elm) => acc.add(elm))
            def map[Y](f: X => Y): LIFOWorkList[Y] = LIFOWorkList(wl.lst.map(f).reverse)
            def toList: List[X] = wl.lst
            def toSet: Set[X] = wl.set
            def filter(f: X => Boolean): LIFOWorkList[X] = LIFOWorkList(wl.lst.filter(f), wl.set.filter(f))
            def filterNot(f: X => Boolean): LIFOWorkList[X] = LIFOWorkList(wl.lst.filterNot(f), wl.set.filterNot(f))
            def contains(x: X): Boolean = wl.set.contains(x)
            def -(x: X): LIFOWorkList[X] = LIFOWorkList(wl.lst.filter(_ != x), wl.set - x)
