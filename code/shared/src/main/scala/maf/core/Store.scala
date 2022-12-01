package maf.core

import maf.util.SmartHash
import maf.util.datastructures.SmartMap
import scala.annotation.tailrec
import maf.lattice.interfaces.LatticeWithAddrs
import maf.modular.scheme._
import maf.lattice.interfaces.BoolLattice

//
// A basic store
//

case class BasicStore[A, V](content: Map[A, V])(using lat: Lattice[V]):
    inline def apply(a: A): V = content(a)
    inline def lookup(a: A): Option[V] = content.get(a)
    inline def extend(a: A, v: V): BasicStore[A, V] = extendOption(a, v).getOrElse(this)
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

object BasicStore:
    def empty = BasicStore(Map.empty)

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

case class Delta[A <: Address, V](sto: LocalStore[A,V], delta: SmartMap[A, (V, AbstractCount)], updates: Set[A])(using lat: LatticeWithAddrs[V, A]) extends AbstractGC[A]:
    def --(ads: Set[A]): Delta[A, V] = Delta(sto, delta -- ads, updates -- ads)
    // abstract GC support
    type This = Delta[A, V]
    def fresh = this.copy(delta = SmartMap.empty)
    def move(addr: A, to: Delta[A, V]): (Delta[A, V], Set[A]) =
        delta.get(addr) match
            case None =>
                sto.content.get(addr) match
                    case None         => (to, Set.empty)
                    case Some((v, _)) => (to, lat.refs(v))
            case Some(s @ (v, _)) => (to.copy(delta = to.delta + (addr -> s)), lat.refs(v))
object Delta:
    def empty[A <: Address, V](sto: LocalStore[A,V])(using lat: LatticeWithAddrs[V, A]): Delta[A, V] = Delta(sto, SmartMap.empty, Set.empty)

case class LocalStore[A <: Address, V](content: SmartMap[A, (V, AbstractCount)])(using lat: LatticeWithAddrs[V, A], shouldCount: A => Boolean) extends AbstractGC[A]:
    inline def get(a: A): Option[(V, AbstractCount)] = content.get(a)
    inline def getValue(a: A): Option[V] = get(a).map(_._1)
    inline def getCount(a: A): Option[AbstractCount] = get(a).map(_._2)
    inline def lookupValue(a: A): V = getValue(a).getOrElse(lat.bottom)
    inline def lookupCount(a: A): AbstractCount = getCount(a).getOrElse(CountZero)
    def update(adr: A, vlu: V): Delta[A, V] = content.get(adr) match
        case None | Some((_, CountZero))    => throw new Exception("Trying to update a non-existing address")
        case Some((_, CountOne))            => Delta(this, SmartMap((adr -> (vlu, CountOne))), Set(adr)) // strong update
        case Some((oldV, CountInf))         => Delta(this, SmartMap(adr -> (lat.join(oldV, vlu), CountInf)), Set(adr)) // weak update
    def extend(adr: A, vlu: V) = content.get(adr) match
        case None if lat.isBottom(vlu) => Delta.empty(this)
        case None                      => Delta(this, SmartMap(adr -> (vlu, countFor(adr))), Set.empty)
        case Some(old @ (oldV, oldC))  => Delta(this, SmartMap(adr -> (lat.join(oldV, vlu), oldC.inc)), Set.empty)
    def joinAt(adr: A, vlu: V, cnt: AbstractCount): Option[LocalStore[A, V]] =
        content.get(adr) match
            case None => Some(LocalStore(content + (adr -> (vlu, cnt))))
            case Some(old @ (oldV, oldC)) =>
                val upd = (lat.join(oldV, vlu), oldC.join(cnt))
                if upd == old then None else Some(LocalStore(content + (adr -> upd)))
    def -(adr: A): LocalStore[A, V] = LocalStore(content - adr)
    def --(ads: Iterable[A]): LocalStore[A, V] = LocalStore(content -- ads)
    private def countFor(a: A): AbstractCount =
        if shouldCount(a) then CountOne else CountInf
    // delta store ops
    def compose(d1: Delta[A, V], d0: Delta[A, V]): Delta[A, V] =
        Delta(d0.sto, d0.delta ++ d1.delta, d0.updates ++ d1.updates.filter(content.contains(_)))
    def integrate(d: Delta[A, V]): LocalStore[A, V] =
        LocalStore(content ++ d.delta)
    def join(d1: Delta[A, V], d2: Delta[A, V]): Delta[A, V] =
        val ads = d1.delta.keys ++ d2.delta.keys
        Delta(
          d1.sto,
          ads.foldLeft(SmartMap.empty)((acc, adr) =>
              lazy val prv = content.get(adr).getOrElse((lat.bottom, CountZero))
              val (vlu1, cnt1) = d1.delta.get(adr).getOrElse(prv)
              val (vlu2, cnt2) = d2.delta.get(adr).getOrElse(prv)
              acc + (adr -> (lat.join(vlu1, vlu2), cnt1.join(cnt2)))
          ),
          d1.updates ++ d2.updates
        )
    def replay(gcs: LocalStore[A, V], d: Delta[A, V]): Delta[A, V] =
        Delta(
          this,
          d.delta.foldLeft(SmartMap.empty) { case (acc, (adr, s @ (v, c))) =>
              if gcs.content.contains(adr) then acc + (adr -> s)
              else
                  content.get(adr) match
                      case None           => acc + (adr -> s)
                      case Some((v2, c2)) => acc + (adr -> ((lat.join(v2, v), c2 + c)))
          },
          d.updates
        )
    // abstract GC support
    type This = LocalStore[A,V]
    def fresh = LocalStore.empty(using lat)
    def move(addr: A, to: This): (This, Set[A]) =
        content.get(addr) match
            case None             => (to, Set.empty)
            case Some(s @ (v, _)) => (LocalStore(to.content + (addr -> s)), lat.refs(v))

object LocalStore:
    def empty[A <: Address, V](using LatticeWithAddrs[V, A], A => Boolean): LocalStore[A,V] =
        LocalStore(SmartMap.empty)
    def from[A <: Address, V](bds: Iterable[(A, V)])(using LatticeWithAddrs[V, A], A => Boolean): LocalStore[A,V] =
        bds.foldLeft(empty: LocalStore[A,V]) { case (acc, (adr, vlu)) => 
            acc.integrate(acc.extend(adr, vlu)) 
        }

//
// ABSTRACT GC
//

trait AbstractGC[A]:
    // needs to be implemented, depending on what is being GC'd
    type This
    def fresh: This 
    def move(addr: A, to: This): (This, Set[A])
    // implements stop-and-copy GC
    def collect(roots: Set[A]): This =
        scan(roots, roots, fresh)
    @tailrec
    private def scan(toMove: Set[A], visited: Set[A], cur: This): This =
        if toMove.isEmpty 
        then cur
        else
            val addr = toMove.head
            val rest = toMove.tail
            val (updated, refs) = move(addr, cur)
            val newRefs = refs.filterNot(visited)
            scan(rest ++ newRefs, visited ++ newRefs, updated)