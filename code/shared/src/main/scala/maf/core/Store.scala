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

trait MapStore[A <: Address, V] extends Store[A, V] { outer =>
  // refine the This type
  type This >: this.type <: MapStore[A, V] { type This = outer.This }
  // stores elements that form a lattice (and therefore can be joined)
  implicit val lattice: Lattice[V]
  // models the store using a Map
  val content: Map[A, V]
  def updateContent(other: Map[A, V]): This
  // create an empty version of this store
  def empty: This = updateContent(Map.empty)
  def move(addr: A, to: This): This = to.updateContent(to.content + (addr -> content(addr)))
  // core operations
  def lookup(a: A): Option[V] = content.get(a)
  def extend(a: A, v: V): This = extendOption(a, v).getOrElse(this)
  def extendOption(a: A, v: V): Option[This] = content.get(a) match {
    case None if lattice.isBottom(v) => None
    case None                        => Some(updateContent(content + (a -> v)))
    case Some(oldValue) =>
      val newValue = lattice.join(oldValue, v)
      if (oldValue == newValue) {
        None
      } else {
        Some(updateContent(content + (a -> newValue)))
      }
  }
}

trait AbstractGC[A <: Address, V] extends MapStore[A, V] { outer =>
  // refine the This type
  type This >: this.type <: AbstractGC[A, V] { type This = outer.This }
  // get all references of a value
  override val lattice: LatticeWithAddrs[V, A]
  // keep track of refs per address
  val cachedRefs: Map[A, Set[A]]
  def refs(addr: A): Set[A] = cachedRefs.getOrElse(addr, Set.empty)
  def updateRefs(newRefs: Map[A, Set[A]]): This
  override def extendOption(a: A, v: V): Option[This] =
    super.extendOption(a, v).map { newStore =>
      // we assume that refs(x U y) = refs(x) U refs(y)
      val newRefs = refs(a) ++ lattice.refs(v)
      newStore.updateRefs(cachedRefs + (a -> newRefs))
    }
  // stop-and-copy style GC
  def collect(roots: Set[A]): This =
    scan(roots, roots, empty)
  @tailrec
  private def scan(toMove: Set[A], moved: Set[A], current: This): This =
    if (toMove.isEmpty) {
      current
    } else {
      val addr = toMove.head
      val rest = toMove.tail
      if (moved(addr)) {
        scan(rest, moved, current)
      } else {
        scan(rest ++ refs(addr), moved + addr, move(addr, current))
      }
    }
  override def empty: This =
    super.empty
      .updateRefs(Map.empty)
  override def move(addr: A, to: This): This =
    super
      .move(addr, to)
      .updateRefs(to.cachedRefs + (addr -> refs(addr)))
}

object AbstractGC {
  def computeRefs[A <: Address, V](content: Map[A, V])(implicit lattice: LatticeWithAddrs[V, A]): Map[A, Set[A]] =
    content.view.mapValues(lattice.refs).toMap
}

case class BasicStore[A <: Address, V](content: Map[A, V], cachedRefs: Map[A, Set[A]])(implicit val lattice: LatticeWithAddrs[V, A])
    extends MapStore[A, V] {
  type This = BasicStore[A, V]
  def updateContent(other: Map[A, V]): This = this.copy(content = other)
  //def updateRefs(newRefs: Map[A, Set[A]]): BasicStore[A, V] = this.copy(cachedRefs = newRefs)
}

object BasicStore {
  def apply[A <: Address, V](content: Map[A, V])(implicit lattice: LatticeWithAddrs[V, A]): BasicStore[A, V] =
    BasicStore(content, AbstractGC.computeRefs(content))
}
