package maf.domain

import maf.domain.lattice.*

import scala.annotation.tailrec

implicit class MapOps[K, V](m: Map[K, V]):
    inline def adjustAt(k: K)(f: Option[V] => V): Map[K, V] = m + (k -> f(m.get(k)))

//
// Store interface
//

trait Store[S, A, V]:
    def empty: S
    def from(b: Iterable[(A,V)]): S = empty.extend(b)
    extension (s: S)
        def lookup(a: A): V
        final def apply(a: A): V = lookup(a)    // for convenience
        def extend(a: A, v: V): S
        def extend(b: Iterable[(A, V)]): S =    // for convenience
            b.foldLeft(s) { case (acc, (a, v)) => acc.extend(a, v) }
        // by default, update is the same as extend
        def update(a: A, v: V): S = extend(a, v)
        // by default, address comparison isn't that precise in the abstract ...
        def addrEq: MaybeEq[A] = new MaybeEq[A]:
            def apply[B: BoolLattice](a1: A, a2: A): B =
                if a1 == a2 then BoolLattice[B].top else BoolLattice[B].inject(false)
        // extra operations to manipulate the store
        // TODO: put in separate "sub" typeclasses?
        def copyTo(a: A, t: S): S
        def values: Set[V]

object Store:

    // a simple instance
    opaque type SimpleStore[A, V] = Map[A, V]
    given simpleInstance[A, V: Lattice]: Store[SimpleStore[A, V], A, V] with
        def empty = Map.empty
        extension (m: SimpleStore[A, V])
            def lookup(a: A) = m.getOrElse(a, Lattice[V].bottom)
            def extend(a: A, v: V) = m.adjustAt(a) {
                case None       => v
                case Some(oldV) => Lattice[V].join(oldV, v)
            }
            def copyTo(a: A, t: SimpleStore[A, V]) = m.get(a) match
                case None       => t
                case Some(v)    => t + (a -> v)
            def values = m.values.toSet

    // a counting instance
    opaque type CountingStore[A, V] = Map[A, (V, AbstractCount)]
    given countingInstance[A, V: Lattice](using shouldCount: A => Boolean): Store[CountingStore[A, V], A, V] with 
        def empty = Map.empty
        extension (s: CountingStore[A, V])
            private def getValue(a: A) = s.get(a).map(_._1)
            private def getCount(a: A) = s.get(a).map(_._2)
            private def freshCount(a: A) = if shouldCount(a) then CountOne else CountInf 
            def lookup(a: A) = getValue(a).getOrElse(Lattice[V].bottom)
            def extend(a: A, v: V) = s.adjustAt(a) {
                case None               => (v, freshCount(a))
                case Some((oldV, oldC)) => (Lattice[V].join(oldV, v), oldC.inc)
            }
            // update is now more precise due to possible strong updates
            override def update(a: A, v: V): CountingStore[A, V] = s.adjustAt(a) {
                case Some((_, CountOne))    => (v, CountOne)                        // strong update
                case Some((oldV, CountInf)) => (Lattice[V].join(oldV, v), CountInf) // weak update 
                case _                      => throw new Exception("Trying to update a non-existant address")
            }
            // address comparison is also more precise
            override def addrEq = new MaybeEq[A]:
                def apply[B: BoolLattice](a1: A, a2: A): B =
                    if a1 == a2 then
                        if s.getCount(a1) == Some(CountOne)
                        then BoolLattice[B].inject(true)
                        else BoolLattice[B].top
                    else BoolLattice[B].inject(false)
            // extra ops
            def copyTo(a: A, t: CountingStore[A, V]) = s.get(a) match
                case None           => t
                case Some(c@(v,_))  => t + (a -> c)
            def values = s.values.map(_._1).toSet


// Abstract GC

trait GC[S, A]:
    extension (s: S)
        def collect(r: Set[A]): S

trait StopAndCopyGC[S, A] extends GC[S, A]:
    def empty: S
    extension (s: S)
        def collect(r: Set[A]) = scan(r, r, empty)
        @tailrec
        private def scan(toMove: Set[A], visited: Set[A], cur: S): S =
            if toMove.isEmpty then cur
            else
                val addr = toMove.head
                val rest = toMove.tail
                val (updated, refs) = move(addr, cur)
                val newRefs = refs.filterNot(visited)
                scan(rest ++ newRefs, visited ++ newRefs, updated)
        protected def move(a: A, to: S): (S, Set[A])


object GC:

    given storeStopAndCopyGC[S, A <: Address, V](using inst: Store[S, A, V], lat: LatticeWithAddrs[V, A]): StopAndCopyGC[S, A] with
        def empty = inst.empty
        extension (s: S)
            protected def move(a: A, t: S) = (s.copyTo(a, t), lat.refs(s(a)))
             

//
// Abstract counting
//

sealed trait AbstractCount:
    def join(other: AbstractCount): AbstractCount
    def +(cnt: => AbstractCount): AbstractCount
    def inc: AbstractCount = this + CountOne
    def subsumes(other: AbstractCount) = this.join(other) == this
case object CountZero extends AbstractCount:
    def join(other: AbstractCount) = other
    def +(cnt: => AbstractCount) = cnt
case object CountOne extends AbstractCount:
    def join(other: AbstractCount) =
        if other == CountZero then this else other
    def +(cnt: => AbstractCount) =
        if cnt == CountZero then this else CountInf
case object CountInf extends AbstractCount:
    def join(other: AbstractCount) = this
    def +(cnt: => AbstractCount) = this
