package maf.core

import maf.util.SmartHash

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash {

  type This >: this.type <: Store[A, V]

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

case class BasicStore[A <: Address, V: Lattice](content: Map[A, V]) extends Store[A, V] {
  type This = BasicStore[A, V]
  def lookup(a: A): Option[V] = content.get(a)
  def extend(a: A, v: V): This = content.get(a) match {
    // case None if v == Lattice[V].bottom => this  <- not necessary
    case None => BasicStore(content + (a -> v))
    case Some(oldValue) =>
      val newValue = Lattice[V].join(oldValue, v)
      if (oldValue == newValue) {
        this
      } else {
        BasicStore(content + (a -> newValue))
      }
  }
}
