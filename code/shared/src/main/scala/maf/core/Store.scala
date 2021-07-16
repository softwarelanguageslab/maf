package maf.core

import maf.util.SmartHash
import scala.annotation.tailrec
import maf.lattice.interfaces.LatticeWithAddrs

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash { store =>
  // This type is the specific subtype of store that `this` belongs to
  type This >: this.type <: Store[A, V] { type This = store.This }

  /** Looks up a value in the store */
  def lookup(a: A): Option[V]

  /** Add a new entry in the store */
  def extend(a: A, v: V): This
  // Derived operations
  def lookupDefault(a: A, default: V): V = lookup(a) match {
    case Some(a) => a
    case None    => default
  }
  def lookupMF(a: A): MayFail[V, Error] = lookup(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }
  def apply(a: A): V = lookup(a).get

  /** Update (strong update if possible) an entry in the store */
  def update(a: A, v: V): This = extend(a, v)

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): This = extend(a, v)
}

//
// STANDARD MAP-BASED STORE
//

case class BasicStore[A <: Address, V: Lattice](content: Map[A,V]) extends Store[A, V] { outer =>
  // refine the This type
  type This = BasicStore[A, V]
  // lookup
  def lookup(a: A): Option[V] = content.get(a)
  // extend
  def extend(a: A, v: V): This = extendOption(a, v).getOrElse(this)
  def extendOption(a: A, v: V): Option[This] = lookup(a) match {
    case None if Lattice[V].isBottom(v) => None
    case None => Some(BasicStore(content + (a -> v)))
    case Some(oldValue) =>
      val newValue = Lattice[V].join(oldValue, v)
      if (oldValue == newValue) {
        None
      } else {
        Some(BasicStore(content + (a -> newValue)))
      }
    }
}

//
// LOCAL STORE, WHICH SUPPORTS ABSTRACT GC (AND -- COMING SOON -- ABSTRACT COUNTING)
//

case class LocalStore[A <: Address, V](content: Map[A, (V, Set[A])])(implicit val lattice: LatticeWithAddrs[V, A]) 
  extends Store[A, V] { outer =>
  // refine the This type
  type This = LocalStore[A, V]
  // lookup
  def lookup(a: A): Option[V] = content.get(a).map(_._1)
  // extend
  def extend(a: A, v: V): This = extendOption(a, v).getOrElse(this)
  def extendOption(a: A, v: V): Option[This] = content.get(a) match {
    case None if lattice.isBottom(v) => None
    case None => Some(LocalStore(content + (a -> ((v, lattice.refs(v))))))
    case Some((oldValue, oldRefs)) =>
      val newValue = lattice.join(oldValue, v)
      if (oldValue == newValue) {
        None
      } else {
        // we assume that refs(X U Y) = refs(X) U refs(Y)
        Some(LocalStore(content + (a -> ((newValue, oldRefs ++ lattice.refs(v))))))
      }
    }
  // stop-and-copy style GC
  def collect(roots: Set[A]): This =
    scan(roots, Set.empty, Map.empty)
  @tailrec
  private def scan(toMove: Set[A], moved: Set[A], current: Map[A, (V, Set[A])]): This =
    if (toMove.isEmpty) {
      LocalStore(current)
    } else {
      val addr = toMove.head
      val rest = toMove.tail
      if (moved(addr)) {
        scan(rest, moved, current)
      } else {
        val (updated, newRefs) = move(addr, current)
        scan(rest ++ newRefs, moved + addr, updated)
      }
    }
  private def move(addr: A, to: Map[A, (V, Set[A])]): (Map[A, (V, Set[A])], Set[A]) = content.get(addr) match {
    case None => (to, Set.empty) 
    case Some(s@(_, refs)) => (to + (addr -> s), refs)
  }
}

object LocalStore {
  def empty[A <: Address, V](implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A,V] = LocalStore(Map.empty)
  def from[A <: Address, V](content: Iterable[(A, V)])(implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A,V] = 
    content.foldLeft(empty)((acc, bnd) => acc.extend(bnd._1, bnd._2))
}

//
// STORE INSTANTIATIONS
//