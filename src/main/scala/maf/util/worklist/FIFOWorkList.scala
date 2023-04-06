package maf.util.worklist

import scala.collection.immutable.Queue

/** A worklist with deterministic breadth-first exploration order. */
case class FIFOWorkList[X](queue: Queue[X], set: Set[X])
object FIFOWorkList:
    def apply[X](xs: Iterable[X]): FIFOWorkList[X] = instance.empty.addAll(xs)
    def apply[X](xs: X*): FIFOWorkList[X] = apply(xs)

    given instance: WorkList[FIFOWorkList] with
        def empty[X]: FIFOWorkList[X] = FIFOWorkList(Queue[X](), Set[X]())

        extension [X](wl: FIFOWorkList[X])
            def isEmpty: Boolean = wl.queue.isEmpty
            def nonEmpty: Boolean = wl.queue.nonEmpty
            def head: X = wl.queue.head
            def tail: FIFOWorkList[X] = FIFOWorkList(wl.queue.tail, wl.set - wl.queue.head)
            def add(x: X): FIFOWorkList[X] = if wl.set.contains(x) then { wl }
            else { FIFOWorkList(wl.queue.enqueue(x), wl.set + x) }
            def addAll(xs: Iterable[X]): FIFOWorkList[X] = xs.foldLeft(wl)((acc, elm) => acc.add(elm))
            def map[Y](f: X => Y): FIFOWorkList[Y] = FIFOWorkList(wl.queue.map(f))
            def toList: List[X] = wl.queue.toList
            def toSet: Set[X] = wl.set
            def filter(f: X => Boolean): FIFOWorkList[X] = FIFOWorkList(wl.queue.filter(f), wl.set.filter(f))
            def filterNot(f: X => Boolean): FIFOWorkList[X] = FIFOWorkList(wl.queue.filterNot(f), wl.set.filterNot(f))
            def contains(x: X): Boolean = wl.set.contains(x)
            def -(x: X): FIFOWorkList[X] = FIFOWorkList(wl.queue.filter(_ != x), wl.set - x)
