package maf.core

import maf.util.SmartHash
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
case object CountOne extends AbstractCount:
    def join(other: AbstractCount) = other
    def +(cnt: => AbstractCount) = CountInf
case object CountInf extends AbstractCount:
    def join(other: AbstractCount) = this
    def +(cnt: => AbstractCount) = this

case class LocalStore[A, V](content: Map[A, (V, AbstractCount)])(using lat: Lattice[V], shouldCount: A => Boolean):
    outer =>
    case class Delta(delta: Map[A, (V, AbstractCount)], updates: Set[A]):
        def parent = outer
    inline def apply(a: A): V = content(a)._1
    def update(adr: A, vlu: V): Delta = content.get(adr) match
        case None => throw new Exception("Trying to update a non-existing address")
        // strong update
        case Some((oldV, CountOne)) if oldV == vlu => Delta(Map.empty, Set(adr))
        case Some((_, CountOne))                   => Delta(Map((adr -> (vlu, CountOne))), Set(adr))
        // weak update
        case Some((oldV, CountInf)) =>
          val newV = lat.join(oldV, vlu)
          if (oldV == newV) then Delta(Map.empty, Set(adr))
          else Delta(Map(adr -> (newV, CountInf)), Set(adr))
    def extend(adr: A, vlu: V) = content.get(adr) match
        case None if lat.isBottom(vlu) => Delta(Map.empty, Set.empty)
        case None                      => Delta(Map(adr -> (vlu, countFor(adr))), Set.empty)
        case Some(old @ (oldV, oldC)) =>
          val updated = (lat.join(oldV, vlu), oldC.inc)
          if old == updated then emptyDelta else Delta(Map(adr -> updated), Set.empty)
    private def countFor(a: A): AbstractCount =
      if shouldCount(a) then CountOne else CountInf
    // delta store ops
    val emptyDelta: Delta = Delta(Map.empty, Set.empty)
    def compose(d1: LocalStore[A, V]#Delta, d0: Delta): Delta =
      Delta(d0.delta ++ d1.delta, d0.updates ++ d1.updates.filter(content.contains(_)))
    def integrate(d: Delta): LocalStore[A, V] =
      LocalStore(content ++ d.delta)
    def join(d1: Delta, d2: Delta): Delta =
      Delta(
        d1.delta.foldLeft(d2.delta) { case (acc, (adr, (vlu, cnt))) =>
          acc.get(adr) match
              case None               => acc + (adr -> (vlu, cnt))
              case Some((accV, accC)) => acc + (adr -> (lat.join(accV, vlu), accC.join(cnt)))
        },
        Set.empty
      )
    def replay(gcs: LocalStore[A, V], d: gcs.Delta): Delta =
      Delta(
        d.delta.foldLeft(Map.empty) { case (acc, (adr, s @ (v, c))) =>
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
      LocalStore(Map.empty)
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
