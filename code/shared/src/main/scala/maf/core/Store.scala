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

case class Delta[A, V](delta: SmartMap[A, (V, AbstractCount)], updates: Set[A]):
    def --(ads: Set[A]): Delta[A, V] = Delta(delta -- ads, updates -- ads)
object Delta:
    def empty[A, V]: Delta[A, V] = Delta(SmartMap.empty, Set.empty)

case class LocalStore[A, V](content: SmartMap[A, (V, AbstractCount)])(using lat: Lattice[V], shouldCount: A => Boolean):
    inline def apply(a: A): V = content(a)._1
    inline def lookup(a: A): Option[V] = content.get(a).map(_._1)
    def update(adr: A, vlu: V): Delta[A, V] = content.get(adr) match
        case None                   => throw new Exception("Trying to update a non-existing address")
        case Some((_, CountOne))    => Delta(SmartMap((adr -> (vlu, CountOne))), Set(adr)) // strong update
        case Some((oldV, CountInf)) => Delta(SmartMap(adr -> (lat.join(oldV, vlu), CountInf)), Set(adr)) // weak update
    def extend(adr: A, vlu: V) = content.get(adr) match
        case None if lat.isBottom(vlu) => Delta.empty
        case None                      => Delta(SmartMap(adr -> (vlu, countFor(adr))), Set.empty)
        case Some(old @ (oldV, oldC))  => Delta(SmartMap(adr -> (lat.join(oldV, vlu), oldC.inc)), Set.empty)
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
      Delta(d0.delta ++ d1.delta, d0.updates ++ d1.updates.filter(content.contains(_)))
    def integrate(d: Delta[A, V]): LocalStore[A, V] =
      LocalStore(content ++ d.delta)
    def join(d1: Delta[A, V], d2: Delta[A, V]): Delta[A, V] =
        val ads = d1.delta.keys ++ d2.delta.keys
        Delta(
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
        d.delta.foldLeft(SmartMap.empty) { case (acc, (adr, s @ (v, c))) =>
          if gcs.content.contains(adr) then acc + (adr -> s)
          else
              content.get(adr) match
                  case None           => acc + (adr -> s)
                  case Some((v2, c2)) => acc + (adr -> ((lat.join(v2, v), c2 + c)))
        },
        d.updates
      )

object LocalStore:
    def empty[A, V](using Lattice[V], A => Boolean) =
      LocalStore(SmartMap.empty)
    def from[A, V](bds: Iterable[(A, V)])(using Lattice[V], A => Boolean) =
      bds.foldLeft(empty) { case (acc, (adr, vlu)) => acc.integrate(acc.extend(adr, vlu)) }

//
// ABSTRACT GC
//

trait AbstractGarbageCollector[X, A]:
    // needs to be implemented, depending on what is being GC'd
    def fresh(cur: X): X
    def move(addr: A, from: X, to: X): (X, Set[A])
    // implements stop-and-copy GC
    def collect(sto: X, roots: Set[A]): X =
      scan(roots, roots, sto, fresh(sto))
    @tailrec
    private def scan(toMove: Set[A], visited: Set[A], prv: X, cur: X): X =
      if toMove.isEmpty then cur
      else
          val addr = toMove.head
          val rest = toMove.tail
          val (updated, refs) = move(addr, prv, cur)
          val newRefs = refs.filterNot(visited)
          scan(rest ++ newRefs, visited ++ newRefs, prv, updated)
