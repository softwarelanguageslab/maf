package maf.domain

import maf.domain.lattice.LatticeWithAddrs
import maf.util.SmartMap

import scala.annotation.tailrec

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
