package maf.core

import maf.util.SmartHash
import scala.annotation.tailrec

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash { store =>

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

  // Allow strong updates if needed.

  /** Update an entry in the store */
  def update(a: A, v: V): This = extend(a, v)

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): This = extend(a, v)
}

trait MapStore[A <: Address, V] extends Store[A, V] { outer => 

  type This >: this.type <: MapStore[A, V] { type This = outer.This }

  implicit val lattice: Lattice[V]

  val content: Map[A, V]
  def updateContent(other: Map[A,V]): This

  def lookup(a: A): Option[V] = content.get(a)
  def extend(a: A, v: V): This = content.get(a) match {
    case None if Lattice[V].isBottom(v) => this
    case None => updateContent(content + (a -> v))
    case Some(oldValue) =>
      val newValue = Lattice[V].join(oldValue, v)
      if (oldValue == newValue) {
        this
      } else {
        updateContent(content + (a -> newValue))
      }
  }
}

trait AbstractGC[A <: Address, V] extends MapStore[A, V] {
  // trim the content of a store
  def trimContent(other: Map[A,V]): This  
  // get all references of a value
  def refs(v: V): Set[A]
  // stop-and-copy style GC
  def collect(roots: Set[A]): This =
    scan(roots, roots, Map.empty)
  @tailrec
  private def scan(toMove: Set[A], moved: Set[A], current: Map[A,V]): This =
    if(toMove.isEmpty) {
      trimContent(current) 
    } else {
      val addr = toMove.head
      val rest = toMove.tail
      if (moved(addr)) {
        scan(rest, moved, current)
      } else {
        val absv = this.content(addr)
        val refs = this.refs(absv)
        scan(rest ++ refs, moved + addr, current + (addr -> absv))
      }
    }
}

case class BasicStore[A <: Address, V: Lattice](content: Map[A,V]) extends MapStore[A, V] {
  type This = BasicStore[A,V]
  val lattice = Lattice[V]
  def updateContent(other: Map[A,V]): BasicStore[A,V] = BasicStore(other)
}
