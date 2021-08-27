package maf.core

import maf.util.SmartHash
import scala.annotation.tailrec
import maf.lattice.interfaces.LatticeWithAddrs
import maf.modular.scheme._
import maf.lattice.interfaces.BoolLattice

case class UnboundAddress[A <: Address](a: A) extends Error

trait Store[A <: Address, V] extends SmartHash { store =>
  // This type is the specific subtype of store that `this` belongs to
  type This >: this.type <: Store[A, V] { type This = store.This }

  /** Looks up a value in the store */
  def lookup(a: A): Option[V]

  /** Add a new entry in the store */
  def extend(a: A, v: V): This
  // Derived operations
  def apply(a: A): V = lookup(a).get
  def lookupDefault(a: A, default: V): V = lookup(a) match {
    case Some(a) => a
    case None    => default
  }
  def lookupMF(a: A): MayFail[V, Error] = lookup(a) match {
    case Some(a) => MayFail.success(a)
    case None    => MayFail.failure(UnboundAddress(a))
  }

  /** Update (strong update if possible) an entry in the store */
  def update(a: A, v: V): This = extend(a, v)

  /** Tries to update an address if it's already mapped into the store. Otherwise, extend the store */
  def updateOrExtend(a: A, v: V): This = extend(a, v)

  /** Join with another store */
  def join(other: This): This
  /* Delta stores represent changes to this store */
  type DeltaStore <: Store[A, V] { type This = store.DeltaStore }
  def deltaStore: DeltaStore
  def integrate(delta: DeltaStore): This

  /** Check if two addresses of the store are equal */
  def addrEq: MaybeEq[A] = new MaybeEq[A] {
    def apply[B: BoolLattice](a1: A, a2: A) =
      if (a1 == a2) {
        BoolLattice[B].top // we don't know (could be different concrete addresses abstracted to the same abstract address)
      } else {
        BoolLattice[B].inject(false) // definitely not the same address
      }
  }
}

//
// MAP-BASED STORES
//

trait MapStore[A <: Address, S, V] extends Store[A, V] { outer =>
  // refine the This type
  type This >: this.type <: MapStore[A, S, V] { type This = outer.This }
  // consists out of a mapping from addresses of type A to elements of type S
  val content: Map[A, S]
  def update(content: Map[A, S]): This
  def get(a: A): Option[S] = content.get(a)
  def empty = update(Map.empty)
  // Elements of type V should form a lattice
  implicit val lattice: Lattice[V]
  // Elements of type S should support the following operations
  def value(s: S): V
  def fresh(a: A, v: V): S
  def extend(s: S, v: V): S
  def join(s1: S, s2: S): S
  // Lookup
  def lookup(a: A): Option[V] = get(a).map(value)
  // Extend
  def extend(a: A, v: V): This = extendOption(a, v).getOrElse(this)
  def extendOption(a: A, v: V): Option[This] = get(a) match {
    case None if lattice.isBottom(v) => None
    case None                        => Some(update(content + (a -> fresh(a, v))))
    case Some(old) =>
      val updated = extend(old, v)
      if (updated == old) {
        None
      } else {
        Some(update(content + (a -> updated)))
      }
  }
  // Join
  def join(other: This): This = update(joinContent(other.content))
  final protected def joinContent(other: Map[A, S]): Map[A, S] =
    other.foldLeft(content) { case (acc, (a, s)) =>
      acc.get(a) match {
        case None       => acc + (a -> s)
        case Some(accS) => acc + (a -> join(accS, s))
      }
    }
  // Delta store
  type DeltaStore <: DeltaMapStore { type This = outer.DeltaStore }
  trait DeltaMapStore extends MapStore[A, S, V] { delta =>
    type This >: this.type <: outer.DeltaMapStore { type This = delta.This }
    override def get(a: A): Option[S] = content.get(a).orElse(outer.get(a))
    def parent: MapStore[A, S, V] = outer
  }
  def integrate(delta: DeltaStore): This =
    update(delta.content.foldLeft(content) { case (acc, (a, s)) => acc + (a -> s) })
}

//
// A SIMPLE STORE (NO ABSTRACT GC OR ABSTRACT COUNTING)
//

trait BasicStoreT[A <: Address, V] extends MapStore[A, V, V] { outer =>
  type This >: this.type <: BasicStoreT[A, V] { type This = outer.This }
  // requires a Lattice[V]
  implicit val lattice: Lattice[V]
  // S = values
  def value(v: V): V = v
  def fresh(a: A, v: V): V = v
  def extend(v1: V, v2: V): V = lattice.join(v1, v2)
  def join(v1: V, v2: V): V = lattice.join(v1, v2)
  type DeltaStore = BasicDeltaStore
  def deltaStore = BasicDeltaStore(Map.empty)
  case class BasicDeltaStore(content: Map[A, V])(implicit val lattice: Lattice[V]) extends DeltaMapStore with BasicStoreT[A, V] {
    type This = outer.BasicDeltaStore
    def update(content: Map[A, V]) = outer.BasicDeltaStore(content)
  }
}

case class BasicStore[A <: Address, V](content: Map[A, V])(implicit val lattice: Lattice[V]) extends BasicStoreT[A, V] { outer =>
  type This = BasicStore[A, V]
  def update(updated: Map[A, V]) = BasicStore(updated)
}

//
// ABSTRACT GC
//

trait AbstractGC[A <: Address, S, V] extends MapStore[A, S, V] { outer =>
  // refine the This type
  type This >: this.type <: AbstractGC[A, S, V] { type This = outer.This }
  // values can contain certain addresses
  implicit override val lattice: LatticeWithAddrs[V, A]
  // need to be able to extract addresses at given address
  def refs(s: S): Set[A]
  // stop-and-copy style GC
  def collect(roots: Set[A]): This =
    scan(roots, Set.empty, Map.empty)
  @tailrec
  private def scan(toMove: Set[A], moved: Set[A], current: Map[A, S]): This =
    if (toMove.isEmpty) {
      update(current)
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
  private def move(addr: A, to: Map[A, S]): (Map[A, S], Set[A]) = content.get(addr) match {
    case None    => (to, Set.empty)
    case Some(s) => (to + (addr -> s), refs(s))
  }
}

//
// ABSTRACT COUNTING
//

sealed trait AbstractCount {
  def join(other: AbstractCount): AbstractCount
  def +(cnt: => AbstractCount): AbstractCount
  def inc: AbstractCount = this + CountOne
}
case object CountOne extends AbstractCount {
  def join(other: AbstractCount) = other
  def +(cnt: => AbstractCount) = CountInf
}
case object CountInf extends AbstractCount {
  def join(other: AbstractCount) = this
  def +(cnt: => AbstractCount) = this
}

trait AbstractCounting[A <: Address, S, V] extends MapStore[A, S, V] { outer =>
  // refine the This type
  type This >: this.type <: AbstractCounting[A, S, V] { type This = outer.This }
  // should store abstract counts
  def count(s: S): AbstractCount
  // can do strong updates iff count == 1
  override def update(a: A, v: V): This = get(a) match {
    case None                            => throw new Exception("Trying to update an unused address")
    case Some(s) if count(s) == CountOne => update(content + (a -> fresh(a, v)))
    case _                               => extend(a, v)
  }

  override def addrEq: MaybeEq[A] = new MaybeEq[A] {
    def apply[B: BoolLattice](a1: A, a2: A): B =
      if (a1 == a2 && get(a1).map(count(_) == CountOne).getOrElse(false)) {
        BoolLattice[B].inject(true)
      } else if (a1 == a2) {
        BoolLattice[B].top
      } else {
        BoolLattice[B].inject(false)
      }
  }
}

//
// LOCAL STORE, WHICH SUPPORTS BOTH ABSTRACT GC AND ABSTRACT COUNTING
//

trait LocalStoreT[A <: Address, V]
    extends MapStore[A, (V, Set[A], AbstractCount), V]
       with AbstractGC[A, (V, Set[A], AbstractCount), V]
       with AbstractCounting[A, (V, Set[A], AbstractCount), V] { outer =>
  type This >: this.type <: LocalStoreT[A, V] { type This = outer.This }
  // S = value + refs(value) + abstract count
  def value(s: (V, Set[A], AbstractCount)): V = s._1
  def refs(s: (V, Set[A], AbstractCount)): Set[A] = s._2
  def count(s: (V, Set[A], AbstractCount)): AbstractCount = s._3
  def fresh(a: A, v: V) = (v, lattice.refs(v), countFor(a))
  def extend(s: (V, Set[A], AbstractCount), v: V) = {
    val newValue = lattice.join(s._1, v)
    if (newValue != s._1) {
      // we assume that refs(X U Y) = refs(X) ++ refs(Y)
      (newValue, s._2 ++ lattice.refs(v), s._3.inc)
    } else {
      (s._1, s._2, s._3.inc)
    }
  }
  def join(s1: (V, Set[A], AbstractCount), s2: (V, Set[A], AbstractCount)) =
    (lattice.join(s1._1, s2._1), s1._2 ++ s2._2, s1._3.join(s2._3))
  // TODO: parameterize this properly instead of hacking it in here
  def countFor(a: A): AbstractCount = a match {
    case _: VarAddr[_] | _: PtrAddr[_] | _: PrmAddr => CountOne
    case _                                          => CountInf
  }
  // delta store
  type DeltaStore = LocalDeltaStore
  def deltaStore = LocalDeltaStore(Map.empty, Set.empty)
  case class LocalDeltaStore(content: Map[A, (V, Set[A], AbstractCount)], updates: Set[A])(implicit val lattice: LatticeWithAddrs[V, A])
      extends DeltaMapStore
         with LocalStoreT[A, V] {
    type This = outer.LocalDeltaStore
    def update(content: Map[A, (V, Set[A], AbstractCount)]) = outer.LocalDeltaStore(content, updates)
    // tracking updated bindings
    private def addUpdated(a: A) = this.copy(updates = updates + a)
    private def addUpdated(a: Iterable[A]) = this.copy(updates = updates ++ a)
    override def update(a: A, v: V) = super.update(a, v).addUpdated(a)
    override def join(other: outer.LocalDeltaStore): outer.LocalDeltaStore = super.join(other).addUpdated(other.updates)
  }
  // d1 'after' d0
  // assumes that d1: sto1.DeltaStore, where sto1 = this.integrate(d0)
  def compose(d1: This#DeltaStore, d0: DeltaStore): DeltaStore = {
    // assert(d1.parent == integrate(d0))
    LocalDeltaStore(d0.content ++ d1.content, d0.updates ++ d1.updates.filter(content.contains(_)))
  }
  // replay changes of d
  // assumes that d: sto.DeltaStore, where sto = this.collect(rs) (for some rs)
  def replay(d: This#DeltaStore): DeltaStore =
    LocalDeltaStore(
      d.content.foldLeft(Map.empty[A, (V, Set[A], AbstractCount)]) { case (acc, (adr, s @ (v, r, c))) =>
        if (d.parent.content.contains(adr)) {
          acc + (adr -> s)
        } else
          get(adr) match {
            case None               => acc + (adr -> s)
            case Some((v2, r2, c2)) => acc + (adr -> ((lattice.join(v2, v), r2 ++ r, c2 + c)))
          }
      },
      d.updates
    )
}

case class LocalStore[A <: Address, V](content: Map[A, (V, Set[A], AbstractCount)])(implicit val lattice: LatticeWithAddrs[V, A])
    extends LocalStoreT[A, V] {
  type This = LocalStore[A, V]
  def update(content: Map[A, (V, Set[A], AbstractCount)]): LocalStore[A, V] = LocalStore(content)
}

object LocalStore {
  def empty[A <: Address, V](implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A, V] = LocalStore(Map.empty)
  def from[A <: Address, V](content: Iterable[(A, V)])(implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A, V] =
    content.foldLeft(empty)((acc, bnd) => acc.extend(bnd._1, bnd._2))
}
