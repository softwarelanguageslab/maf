package maf.util

import scala.collection.immutable.Queue

object FunctionUtils:
    trait WL[W[_]]:
        outer =>
        def append[T](previous: W[T], ts: Iterable[T]): W[T]
        def remove[T](from: W[T]): (T, W[T])
        def toSet[T](from: W[T]): Set[T]
        def isEmpty[T](from: W[T]): Boolean

    object WL:
        def apply[W[_]](using wl: WL[W]): WL[W] = wl

        extension [W[_], T](worklist: W[T])(using WL[W])
            def ++(ts: Iterable[T]): W[T] = WL[W].append(worklist, ts)
            def remove: (T, W[T]) = WL[W].remove(worklist)
            def isEmpty: Boolean = WL[W].isEmpty(worklist)
            def addNext(f: T => Iterable[T]): W[T] =
                // remove from the WL until its empty
                def loop(worklist: W[T]): W[T] =
                    if { val (_, next) = worklist.remove; next.isEmpty } then
                        val (item, wl) = worklist.remove
                        wl ++ f(item)
                    else
                        val (item, next) = worklist.remove
                        loop(next) ++ f(item)

                loop(worklist)

            def toSet: Set[T] = WL[W].toSet(worklist)

    case class FIFOWL[X](queue: Queue[X], inQueue: Set[X], visited: Set[X])
    object FIFOWL:
        def initial[T](t: T): FIFOWL[T] = FIFOWL(queue = Queue().enqueue(t), inQueue = Set(t), Set())
        given WL[FIFOWL] with
            def append[T](previous: FIFOWL[T], ts: Iterable[T]): FIFOWL[T] =
                ts.foldLeft(previous)((acc, t) =>
                    if acc.inQueue.contains(t) || acc.visited.contains(t) then acc
                    else acc.copy(queue = acc.queue.enqueue(t), inQueue = acc.inQueue + t)
                )

            def remove[T](from: FIFOWL[T]): (T, FIFOWL[T]) =
                val (element, nextQueue) = from.queue.dequeue
                (element, from.copy(queue = nextQueue, inQueue = from.inQueue - element, visited = from.visited + element))

            def isEmpty[T](worklist: FIFOWL[T]): Boolean = worklist.queue.isEmpty
            def toSet[T](from: FIFOWL[T]): Set[T] = from.visited

    /**
     * Adds the "after" operator so that it runs function composition. Example <code>extract after eval</code> first runs eval and then extract on the
     * output of eval
     */
    extension [A, C](f: C => A)
        def after[B](g: B => C): B => A = (b) => f(g(b))
        def andThen[B](g: A => B): C => B = (c) => g(f(c))

    def fix[T](init: T)(f: T => T): T =
        def loop(prev: T): T =
            val next = f(prev)
            if next == prev then next else loop(next)

        loop(init)

    /** Fixpoint computation with a worklist algorithm */
    def fixWL[W[_], T](init: W[T])(f: W[T] => W[T])(using WL[W]): Set[T] =
        import WL.*
        def loop(prev: W[T]): W[T] =
            if prev.isEmpty then prev else loop(f(prev))

        loop(init).toSet
