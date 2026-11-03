package maf.core.worklist

import scala.collection.immutable.Queue

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
