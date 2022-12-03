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

case class LocalStore[A <: Address, V](content: SmartMap[A, (V, AbstractCount)])(using lat: LatticeWithAddrs[V, A], shouldCount: A => Boolean)
    extends AbstractGC[A]:
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
    def extend(adr: A, vlu: V): Delta = content.get(adr) match
        case None if lat.isBottom(vlu) => emptyDelta
        case None                      => Delta(SmartMap(adr -> (vlu, countFor(adr))), Set.empty)
        case Some(old @ (oldV, oldC))  => Delta(SmartMap(adr -> (lat.join(oldV, vlu), oldC.inc)), Set.empty)
    def update(adr: A, vlu: V): Delta = content.get(adr) match
        case None | Some((_, CountZero)) => throw new Exception("Trying to update a non-existing address")
        case Some((_, CountOne))         => Delta(SmartMap((adr -> (vlu, CountOne))), Set(adr)) // strong update
        case Some((oldV, CountInf))      => Delta(SmartMap(adr -> (lat.join(oldV, vlu), CountInf)), Set(adr)) // weak update
    // join a (value, count) at a given address
    def joinAt(adr: A, vlu: V, cnt: AbstractCount): Option[LocalStore[A, V]] =
        get(adr) match
            case None => Some(LocalStore(content + (adr -> (vlu, cnt))))
            case Some(old @ (oldV, oldC)) =>
                val upd = (lat.join(oldV, vlu), oldC.join(cnt))
                if upd == old then None else Some(LocalStore(content + (adr -> upd)))
    // removing addresses from the store
    def -(adr: A): LocalStore[A, V] = LocalStore(content - adr)
    def --(ads: Iterable[A]): LocalStore[A, V] = LocalStore(content -- ads)
    // where to enable abstract counting
    private def countFor(a: A): AbstractCount =
        if shouldCount(a) then CountOne else CountInf
    // integrating a delta in the store
    def integrate(d: Delta): LocalStore[A, V] =
        LocalStore(content ++ d.delta)
    // composing two deltas
    // assumes that (d1 :: nxt.Delta), where nxt = this.integrate(d0)
    def compose(d1: LocalStore[A, V]#Delta, d0: Delta): Delta =
        Delta(d0.delta ++ d1.delta, d0.updates ++ d1.updates.filter(content.contains(_)))
    // joining two deltas
    def join(d1: Delta, d2: Delta): Delta =
        val ads = d1.delta.keys ++ d2.delta.keys
        Delta(
          ads.foldLeft(SmartMap.empty)((acc, adr) =>
              lazy val prv = get(adr).getOrElse((lat.bottom, CountZero))
              val (vlu1, cnt1) = d1.delta.get(adr).getOrElse(prv)
              val (vlu2, cnt2) = d2.delta.get(adr).getOrElse(prv)
              acc + (adr -> (lat.join(vlu1, vlu2), cnt1.join(cnt2)))
          ),
          d1.updates ++ d2.updates
        )
    // replaying a delta that was computed w.r.t. a GC'd store dgc
    // assumes that (dgc :: sgc.Delta), where sgc = this.collect(r) for some r
    def replay(dgc: LocalStore[A, V]#Delta): Delta =
        Delta(
          dgc.delta.map { case (adr, s @ (v, c)) =>
              get(adr) match
                  case None                        => (adr, s)
                  case Some(_) if dgc.inStore(adr) => (adr, s)
                  case Some((v2, c2))              => (adr, (lat.join(v2, v), c2 + c))
          },
          dgc.updates
        )
    // abstract GC support
    type This = LocalStore[A, V]
    def fresh = LocalStore.empty
    def move(addr: A, to: This): (This, Set[A]) =
        content.get(addr) match
            case None             => (to, Set.empty)
            case Some(s @ (v, _)) => (LocalStore(to.content + (addr -> s)), lat.refs(v))
    // Deltas!
    def emptyDelta: Delta = Delta(SmartMap.empty, Set.empty)
    case class Delta(delta: SmartMap[A, (V, AbstractCount)], updates: Set[A]) extends AbstractGC[A]:
        def --(ads: Set[A]): Delta = Delta(delta -- ads, updates -- ads)
        def inStore(a: A): Boolean = sto.content.contains(a)
        // abstract GC support
        type This = Delta
        def fresh = this.copy(delta = SmartMap.empty)
        def move(addr: A, to: Delta): (Delta, Set[A]) =
            delta.get(addr) match
                case None =>
                    sto.get(addr) match
                        case None         => (to, Set.empty)
                        case Some((v, _)) => (to, lat.refs(v))
                case Some(s @ (v, _)) => (to.copy(delta = to.delta + (addr -> s)), lat.refs(v))

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
        if toMove.isEmpty then cur
        else
            val addr = toMove.head
            val rest = toMove.tail
            val (updated, refs) = move(addr, cur)
            val newRefs = refs.filterNot(visited)
            scan(rest ++ newRefs, visited ++ newRefs, updated)
