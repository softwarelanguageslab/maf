package maf.fixpoint.worklist

import scala.collection.immutable.Queue

/**
 * A typeclass for the worklists of type `W[X]`, which contain elements of type `X`, each at most once. A worklist decides the order in which its
 * elements are taken out of it.
 */
trait Worklist[W[_]]:

    /** Returns an empty worklist. */
    def empty[X]: W[X]

    extension [X](wl: W[X])

        /** Returns whether the worklist is empty. */
        def isEmpty: Boolean

        /** Returns the worklist with `x` added to it, if it does not contain `x` yet (otherwise, returns the worklist itself). */
        def add(x: X): W[X]

        /** Returns the next element of the worklist (which must not be empty), and the worklist without it. */
        def next: (X, W[X])

/** A worklist with a breadth-first order: its elements are taken out in the order in which they were added. */
case class FIFOWorklist[X](queue: Queue[X], set: Set[X])

object FIFOWorklist:
    given Worklist[FIFOWorklist] with
        def empty[X]: FIFOWorklist[X] = FIFOWorklist(Queue(), Set())
        extension [X](wl: FIFOWorklist[X])
            def isEmpty: Boolean = wl.queue.isEmpty
            def add(x: X): FIFOWorklist[X] = if wl.set.contains(x) then wl else FIFOWorklist(wl.queue.enqueue(x), wl.set + x)
            def next: (X, FIFOWorklist[X]) =
                val (x, rest) = wl.queue.dequeue
                (x, FIFOWorklist(rest, wl.set - x))

/** A worklist with a depth-first order: its elements are taken out in the reverse order in which they were added. */
case class LIFOWorklist[X](stack: List[X], set: Set[X])

object LIFOWorklist:
    given Worklist[LIFOWorklist] with
        def empty[X]: LIFOWorklist[X] = LIFOWorklist(Nil, Set())
        extension [X](wl: LIFOWorklist[X])
            def isEmpty: Boolean = wl.stack.isEmpty
            def add(x: X): LIFOWorklist[X] = if wl.set.contains(x) then wl else LIFOWorklist(x :: wl.stack, wl.set + x)
            def next: (X, LIFOWorklist[X]) = (wl.stack.head, LIFOWorklist(wl.stack.tail, wl.set - wl.stack.head))

/** A worklist with an arbitrary order. */
case class RandomWorklist[X](set: Set[X])

object RandomWorklist:
    given Worklist[RandomWorklist] with
        def empty[X]: RandomWorklist[X] = RandomWorklist(Set())
        extension [X](wl: RandomWorklist[X])
            def isEmpty: Boolean = wl.set.isEmpty
            def add(x: X): RandomWorklist[X] = RandomWorklist(wl.set + x)
            def next: (X, RandomWorklist[X]) = (wl.set.head, RandomWorklist(wl.set - wl.set.head))
