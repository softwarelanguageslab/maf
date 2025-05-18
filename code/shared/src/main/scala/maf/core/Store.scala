package maf.core

import maf.util.SmartHash
import maf.util.datastructures.SmartMap
import scala.annotation.tailrec
import maf.lattice.interfaces.LatticeWithAddrs
import maf.modular.scheme._
import maf.lattice.interfaces.BoolLattice

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
//
// LEGACY CODE BELOW ... (TODO: refactor everything to use new typeclass instead)
//
//

//
// A basic store
//

case class BasicStore[A <: Address, V](content: Map[A, V])(using lat: LatticeWithAddrs[V, A]):
    inline def apply(a: A): V = lookup(a).getOrElse(lat.bottom)
    inline def lookup(a: A): Option[V] = content.get(a)
    inline def extend(a: A, v: V): BasicStore[A, V] = extendOption(a, v).getOrElse(this)
    inline def extend(bds: Iterable[(A,V)]): BasicStore[A, V] = bds.foldLeft(this) { case (acc, (a,v)) => acc.extend(a,v) }
    def extendOption(a: A, v: V): Option[BasicStore[A, V]] =
        content.get(a) match
            case None if lat.isBottom(v) => None
            case None                    => Some(BasicStore(content + (a -> v)))
            case Some(oldV) =>
                val updated = lat.join(oldV, v)
                if oldV == updated then None
                else Some(BasicStore(content + (a -> updated)))
    def update(a: A, v: V): BasicStore[A, V] =
        content.get(a) match
            case None    => throw new Exception("Attempting to update a non-existing address")
            case Some(_) => extend(a, v)
    // abstract GC support
    type This = BasicStore[A, V]
    def fresh = BasicStore.empty
    def move(addr: A, to: BasicStore[A, V]): (BasicStore[A, V], Set[A]) =
        content.get(addr) match
            case None => (to, Set.empty)
            case Some(v) => (BasicStore(to.content + (addr -> v)), lat.refs(v))
        

object BasicStore:
    def empty[A <: Address, V](using LatticeWithAddrs[V, A]): BasicStore[A,V] = BasicStore(Map.empty)

//
// A store with abstract counting
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

case class CountingStore[A <: Address, V](content: Map[A, (V, AbstractCount)])(using lat: LatticeWithAddrs[V, A], shouldCount: A => Boolean):
    // "get"-functions return an Option
    inline def get(a: A): Option[(V, AbstractCount)] = content.get(a)
    inline def getValue(a: A): Option[V] = get(a).map(_._1)
    inline def getCount(a: A): Option[AbstractCount] = get(a).map(_._2)
    // lookup functions return bottom if not found
    inline def lookup(a: A): (V, AbstractCount) = get(a).getOrElse((lat.bottom, CountZero))
    inline def lookupValue(a: A): V = getValue(a).getOrElse(lat.bottom)
    inline def lookupCount(a: A): AbstractCount = getCount(a).getOrElse(CountZero)
    inline def apply(a: A) = lookupValue(a)
    inline def extend(a: A, v: V): CountingStore[A, V] = extendOption(a, v).getOrElse(this)
    inline def extend(bds: Iterable[(A,V)]): CountingStore[A, V] = bds.foldLeft(this) { case (acc, (a,v)) => acc.extend(a,v) }
    def extendOption(a: A, v: V): Option[CountingStore[A, V]] =
        content.get(a) match
            case None if lat.isBottom(v)    => None
            case None                       => Some(CountingStore(content + (a -> (v, countFor(a)))))
            case Some(old @ (oldV, oldC))   =>
                val updated = (lat.join(oldV, v), oldC.inc)
                if oldV == updated then None else Some(CountingStore(content + (a -> updated)))
    def update(a: A, v: V): CountingStore[A, V] =
        content.get(a) match
            case None | Some((_, CountZero))    => throw new Exception("Attempting to update a non-existing address")
            case Some((_, CountOne))            => CountingStore(content + (a -> (v, CountOne)))                    // strong update
            case Some((oldV, CountInf))         => CountingStore(content + (a -> (lat.join(oldV, v), CountInf)))    // weak update
    // Abstract counting GC
    private def countFor(a: A): AbstractCount =
        if shouldCount(a) then CountOne else CountInf
    // Abstract GC support
    type This = CountingStore[A, V]
    def fresh = CountingStore.empty
    def move(addr: A, to: CountingStore[A,V]): (This, Set[A]) = 
        content.get(addr) match
            case None => (to, Set.empty)
            case Some(s @ (v, _)) => (CountingStore(to.content + (addr -> s)), lat.refs(v)) 

object CountingStore:
    def empty[A <: Address, V](using LatticeWithAddrs[V, A], A => Boolean): CountingStore[A, V] = CountingStore(Map.empty)

//
// for DSS: counting + delta stores
//

case class Delta[A <: Address, V](delta: SmartMap[A, (V, AbstractCount)])

case class DeltaGC[A <: Address,V](sto: LocalStore[A,V])(using lat: LatticeWithAddrs[V, A]) extends AbstractGC[Delta[A,V], A]:
    def fresh = Delta.emptyDelta[A,V]
    def move(addr: A, from: Delta[A,V], to: Delta[A,V]): (Delta[A,V], Set[A]) =
        from.delta.get(addr) match
            case None =>
                sto.get(addr) match
                    case None         => (to, Set.empty)
                    case Some((v, _)) => (to, lat.refs(v))
            case Some(s @ (v, _)) => (Delta(to.delta + (addr -> s)), lat.refs(v))

object Delta:
    // Deltas!
    def emptyDelta[A <: Address,V]: Delta[A,V] = Delta(SmartMap.empty)
    def compose[A <: Address,V](d1:Delta[A,V], d0: Delta[A,V]): Delta[A,V] =
        Delta(d0.delta ++ d1.delta)

case class LocalStore[A <: Address, V](content: SmartMap[A, (V, AbstractCount)])(using lat: LatticeWithAddrs[V, A], shouldCount: A => Boolean):
    sto =>
    // "get"-functions return an Option
    inline def get(a: A): Option[(V, AbstractCount)] = content.get(a)
    inline def getValue(a: A): Option[V] = get(a).map(_._1)
    inline def getCount(a: A): Option[AbstractCount] = get(a).map(_._2)
    // lookup functions return bottom if not found
    inline def lookup(a: A): (V, AbstractCount) = get(a).getOrElse((lat.bottom, CountZero))
    inline def lookupValue(a: A): V = getValue(a).getOrElse(lat.bottom)
    inline def lookupCount(a: A): AbstractCount = getCount(a).getOrElse(CountZero)
    // extend & update return a Delta
    def extend(adr: A, vlu: V): Delta[A,V] = content.get(adr) match
        case None if lat.isBottom(vlu) => Delta.emptyDelta[A,V]
        case None                      => Delta(SmartMap(adr -> (vlu, countFor(adr))))
        case Some(old @ (oldV, oldC))  => Delta(SmartMap(adr -> (lat.join(oldV, vlu), oldC.inc)))
    def update(adr: A, vlu: V): Delta[A,V] = content.get(adr) match
        case None | Some((_, CountZero)) => throw new Exception("Trying to update a non-existing address")
        case Some((_, CountOne))         => Delta(SmartMap(adr -> (vlu, CountOne))) // strong update
        case Some((oldV, CountInf))      => Delta(SmartMap(adr -> (lat.join(oldV, vlu), CountInf))) // weak update
    // join a (value, count) at a given address
    def joinAt(adr: A, vlu: V, cnt: AbstractCount): Option[LocalStore[A, V]] =
        get(adr) match
            case None => Some(LocalStore(content + (adr -> (vlu, cnt))))
            case Some(old @ (oldV, oldC)) =>
                val upd = (lat.join(oldV, vlu), oldC.join(cnt))
                if upd == old then None else Some(LocalStore(content + (adr -> upd)))
    // contains check
    def contains(adr: A): Boolean =
        content.contains(adr)
    // removing addresses from the store
    def -(adr: A): LocalStore[A, V] = LocalStore(content - adr)
    def --(ads: Iterable[A]): LocalStore[A, V] = LocalStore(content -- ads)
    // where to enable abstract counting
    private def countFor(a: A): AbstractCount =
        if shouldCount(a) then CountOne else CountInf
    // integrating a delta in the store
    def integrate(d: Delta[A, V]): LocalStore[A, V] =
        LocalStore(content ++ d.delta)
    // joining two deltas
    def join(d1: Delta[A,V], d2: Delta[A,V]): Delta[A,V] =
        val ads = d1.delta.keys ++ d2.delta.keys
        Delta(ads.foldLeft(SmartMap.empty)((acc, adr) =>
            lazy val prv = get(adr).getOrElse((lat.bottom, CountZero))
            val (vlu1, cnt1) = d1.delta.get(adr).getOrElse(prv)
            val (vlu2, cnt2) = d2.delta.get(adr).getOrElse(prv)
            acc + (adr -> (lat.join(vlu1, vlu2), cnt1.join(cnt2)))
        ))
    // replaying a delta that was computed w.r.t. a GC'd store
    def replay(delta: Delta[A,V], allocated: Set[A]): Delta[A,V] =
        Delta(delta.delta.map { case bnd @ (adr, (v, c)) =>
            if allocated.contains(adr) then 
                get(adr) match {
                    case None           => bnd 
                    case Some((v2, c2)) => (adr, (lat.join(v2, v), CountInf))
                }
            else 
                bnd 
        })
    override def toString = 
        content.content
               .filter { case (a, (v, c)) => a.printable }
               .toString

case class LocalStoreGC[A <: Address,V]()(using lat: LatticeWithAddrs[V, A], shouldCount: A=>Boolean) extends AbstractGC[LocalStore[A,V], A]:
    def fresh = LocalStore.empty[A,V]
    def move(addr: A, from: LocalStore[A,V], to: LocalStore[A,V]): (LocalStore[A,V], Set[A]) =
        from.get(addr) match
            case None             => (to, Set.empty)
            case Some(s @ (v, _)) => (LocalStore[A,V](to.content + (addr -> s)), lat.refs(v))

object LocalStore:
    def empty[A <: Address, V](using LatticeWithAddrs[V, A], A => Boolean): LocalStore[A, V] =
        LocalStore(SmartMap.empty)
    def from[A <: Address, V](bds: Iterable[(A, V)])(using LatticeWithAddrs[V, A], A => Boolean): LocalStore[A, V] =
        bds.foldLeft(empty: LocalStore[A, V]) { case (acc, (adr, vlu)) =>
            acc.integrate(acc.extend(adr, vlu))
        }

//
// ABSTRACT GC
//

trait AbstractGC[S, A]: 
    // needs to be implemented, depending on what is being GC'd
    def fresh: S
    def move(addr: A, from: S, to: S): (S, Set[A])
    // implements stop-and-copy GC
    def collect(store: S, roots: Set[A]): S =
        scan(store, roots, roots, fresh)
    @tailrec
    private def scan(prv: S, toMove: Set[A], visited: Set[A], cur: S): S =
        if toMove.isEmpty then cur
        else
            val addr = toMove.head
            val rest = toMove.tail
            val (updated, refs) = move(addr, prv, cur)
            val newRefs = refs.filterNot(visited)
            scan(prv, rest ++ newRefs, visited ++ newRefs, updated)
