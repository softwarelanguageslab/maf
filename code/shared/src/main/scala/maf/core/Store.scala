package maf.core

import maf.util.SmartHash
import scala.annotation.tailrec
import maf.lattice.interfaces.LatticeWithAddrs
import maf.modular.scheme._
import maf.lattice.interfaces.BoolLattice

trait Store:
    type Delta

trait StoreOps[S <: Store, A, V](using Lattice[V]):
    // core store operations 
    def empty: S
    def extend(s: S, a: A, v: V): s.Delta
    def update(s: S, a: A, v: V): s.Delta = extend(s, a, v)
    def lookup(s: S, a: A): Option[V]
    extension (s: S) def apply(a: A) = lookup(s, a).get
    // store can contain extra information (e.g., abstract counts) to determine equality of addresses
    // TODO[maybe]: move this somewhere else?
    def addrEq(s: S): MaybeEq[A] = 
      new MaybeEq[A] {
        def apply[B: BoolLattice](a1: A, a2: A) =
          if a1 == a2 then BoolLattice[B].top // we don't know (could be different concrete addresses abstracted to the same abstract address)
          else BoolLattice[B].inject(false)   // definitely not the same address
      }
    // delta store ops
    def delta(sto: S): sto.Delta
    def integrate(sto: S, delta: sto.Delta): S
    def compose(sto1: S, d1: sto1.Delta)(sto0: S, d0: sto0.Delta): sto0.Delta  // d1 `after` d0
    def join(sto: S, d1: sto.Delta, d2: sto.Delta): sto.Delta   // d1 `join` d2

//
// A basic store
//

case class BasicStore[A, V](content: Map[A, V]) extends Store:
    type Delta = BasicStore[A, V]

given BasicStoreOps[A, V](using lat: Lattice[V]): StoreOps[BasicStore[A, V], A, V] with
    type S = BasicStore[A, V]
    // delta
    opaque type Delta[_] = BasicStore[A, V]
    // core ops
    def empty = BasicStore(Map.empty)
    def lookup(s: BasicStore[A, V], a: A) = s.content.get(a)
    def extend(s: BasicStore[A, V], a: A, v: V) = extendOption(s, a, v).getOrElse(s)
    def extendOption(s: BasicStore[A, V], a: A, v: V): Option[BasicStore[A,V]] = 
        s.content.get(a) match
            case None if lat.isBottom(v) => None
            case None => Some(BasicStore(s.content + (a -> v)))
            case Some(oldV) =>
                val updated = lat.join(oldV, v)
                if oldV == updated then
                    None
                else
                    Some(BasicStore(s.content + (a -> updated)))
    def compose(sto1: S, d1: sto1.Delta)(sto0: S, d0: sto0.Delta) = d1
    def delta(sto: S) = sto
    def integrate(sto: S, delta: sto.Delta) = delta
    def join(sto: S, d1: sto.Delta, d2: sto.Delta) =
        throw new Exception("Join not supported here -- that would be too slow!")

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

case class CountingStore[A,V](content: Map[A, (V, AbstractCount)]) extends Store:
    case class Delta(delta: Map[A, (V, AbstractCount)], updates: Set[A])

given CountingStoreOps[A,V](using lat: Lattice[V], shouldCount: A => Boolean): StoreOps[CountingStore[A,V], A, V] with
    // core operations
    def empty = CountingStore(Map.empty)
    def from(bds: Iterable[(A,V)]) = CountingStore(bds.map((a,v) => (a -> (v, countFor(a)))).toMap)
    def lookup(s: CountingStore[A,V], a: A) = s.content.get(a).map(_._1)
    override def update(sto: CountingStore[A,V], adr: A, vlu: V) =
      sto.content.get(adr) match
        case None => throw new Exception("Trying to update a non-existing address")
        // strong update
        case Some((oldV, CountOne)) if oldV == vlu => sto.Delta(Map.empty, Set(adr))
        case Some((_, CountOne)) => sto.Delta(Map((adr -> (vlu, CountOne))), Set(adr))
        // weak update
        case Some((oldV, CountInf)) =>
            val newV = lat.join(oldV, vlu)
            if (oldV == newV) then sto.Delta(Map.empty, Set(adr))
            else sto.Delta(Map(adr -> (newV, CountInf)), Set(adr))
    def extend(sto: CountingStore[A, V], adr: A, vlu: V) = 
      sto.content.get(adr) match
        case None if lat.isBottom(vlu) => sto.Delta(Map.empty, Set.empty)
        case None => sto.Delta(Map(adr -> (vlu, countFor(adr))), Set.empty)
        case Some((oldV, CountOne)) => sto.Delta(Map(adr -> (lat.join(oldV, vlu), CountInf)), Set.empty)
        case Some((oldV, CountInf)) =>
            val newV = lat.join(oldV, vlu)
            if (oldV == newV) then sto.Delta(Map.empty, Set.empty)
            else sto.Delta(Map(adr -> (newV, CountInf)), Set.empty)
    private def countFor(a: A): AbstractCount = 
      if shouldCount(a) then CountOne else CountInf
    override def addrEq(s: CountingStore[A, V]): MaybeEq[A] = 
        new MaybeEq[A] {
            def apply[B: BoolLattice](a1: A, a2: A): B =
                if a1 == a2 then 
                    s.content.get(a1) match 
                        case Some((_, CountOne)) => BoolLattice[B].inject(true)
                        case _ => BoolLattice[B].top
                else BoolLattice[B].inject(false)
        }
    def compose(sto1: CountingStore[A, V], d1: sto1.Delta)(sto0: CountingStore[A, V], d0: sto0.Delta): sto0.Delta =
        sto0.Delta(d0.delta ++ d1.delta, d0.updates ++ d1.updates.filter(sto0.content.contains(_)))
    def delta(sto: CountingStore[A, V]): sto.Delta =
        sto.Delta(Map.empty, Set.empty)
    def integrate(sto: CountingStore[A, V], delta: sto.Delta): CountingStore[A, V] =
        CountingStore(sto.content ++ delta.delta)
    def join(sto: CountingStore[A, V], d1: sto.Delta, d2: sto.Delta): sto.Delta =
        sto.Delta(d1.delta.foldLeft(d2.delta) { case (acc, (adr, (vlu, cnt))) =>
            acc.get(adr) match
                case None => acc + (adr -> (vlu, cnt))
                case Some((accV, accC)) => acc + (adr -> (lat.join(accV, vlu), accC.join(cnt))) 
        }, Set.empty)
    
/*

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
  case class BasicDeltaStore(content: Map[A, V])(implicit val lattice: Lattice[V]) extends DeltaMapStore with BasicStoreT[A, V]:
      type This = outer.BasicDeltaStore
      def bind(adr: A, vlu: V) = outer.BasicDeltaStore(content + (adr -> vlu))
}

case class BasicStore[A <: Address, V](content: Map[A, V])(implicit val lattice: Lattice[V]) extends BasicStoreT[A, V] { outer =>
  type This = BasicStore[A, V]
  def bind(adr: A, vlu: V) = BasicStore(content + (adr -> vlu))
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
  // need to be able to make an empty instance
  def empty: This
  // stop-and-copy style GC
  def collect(roots: Set[A]): This =
    scan(roots, Set.empty, empty)
  @tailrec
  private def scan(toMove: Set[A], moved: Set[A], current: This): This =
    if toMove.isEmpty then current
    else
        val addr = toMove.head
        val rest = toMove.tail
        if moved(addr) then scan(rest, moved, current)
        else
            val (updated, newRefs) = move(addr, current)
            scan(rest ++ newRefs, moved + addr, updated)
  private def move(addr: A, to: This): (This, Set[A]) = get(addr) match
      case None                              => (to, Set.empty)
      case Some(s) if content.contains(addr) => (to.bind(addr, s), refs(s))
      case Some(s)                           => (to, refs(s))
}

//
// ABSTRACT COUNTING
//

trait AbstractCounting[A <: Address, S, V] extends MapStore[A, S, V] { outer =>
  // refine the This type
  type This >: this.type <: AbstractCounting[A, S, V] { type This = outer.This }
  // should store abstract counts
  def count(s: S): AbstractCount
  // determine the initial abstract count for a given address
  // if CountInf is used, abstract counting is disabled
  def enableCounting(a: A): Boolean
  def countFor(a: A): AbstractCount =
    if enableCounting(a) then CountOne else CountInf
  // can do strong updates iff count == 1
  override def update(a: A, v: V): This = get(a) match
      case None                            => throw new Exception("Trying to update an unused address")
      case Some(s) if count(s) == CountOne => bind(a, fresh(a, v))
      case _                               => extend(a, v)

  override def addrEq: MaybeEq[A] = new MaybeEq[A] {
    def apply[B: BoolLattice](a1: A, a2: A): B =
      if a1 == a2 && get(a1).map(count(_) == CountOne).getOrElse(false) then BoolLattice[B].inject(true)
      else if a1 == a2 then BoolLattice[B].top
      else BoolLattice[B].inject(false)
  }
}

//
// LOCAL STORE, WHICH SUPPORTS BOTH ABSTRACT GC AND ABSTRACT COUNTING
//

trait LocalStoreT[A <: Address, V](shouldCount: A => Boolean)
    extends MapStore[A, (V, Set[A], AbstractCount), V]
    with AbstractGC[A, (V, Set[A], AbstractCount), V]
    with AbstractCounting[A, (V, Set[A], AbstractCount), V] { outer =>
  type This >: this.type <: LocalStoreT[A, V] { type This = outer.This }
  // S = value + refs(value) + abstract count
  def value(s: (V, Set[A], AbstractCount)): V = s._1
  def refs(s: (V, Set[A], AbstractCount)): Set[A] = s._2
  def count(s: (V, Set[A], AbstractCount)): AbstractCount = s._3
  def fresh(a: A, v: V) = (v, lattice.refs(v), countFor(a))
  def extend(s: (V, Set[A], AbstractCount), v: V) =
      val newValue = lattice.join(s._1, v)
      if newValue != s._1 then
          // we assume that refs(X U Y) = refs(X) ++ refs(Y)
          (newValue, s._2 ++ lattice.refs(v), s._3.inc)
      else (s._1, s._2, s._3.inc)
  def join(s1: (V, Set[A], AbstractCount), s2: (V, Set[A], AbstractCount)) =
    (lattice.join(s1._1, s2._1), s1._2 ++ s2._2, s1._3.join(s2._3))
  def enableCounting(a: A) = shouldCount(a)
  // delta store
  type DeltaStore = LocalDeltaStore
  def deltaStore = LocalDeltaStore(Map.empty, Set.empty)
  case class LocalDeltaStore(content: Map[A, (V, Set[A], AbstractCount)], updates: Set[A])(implicit val lattice: LatticeWithAddrs[V, A])
      extends DeltaMapStore
      with LocalStoreT[A, V](shouldCount):
      type This = outer.LocalDeltaStore
      def empty = outer.LocalDeltaStore(Map.empty, Set.empty)
      def bind(a: A, s: (V, Set[A], AbstractCount)) = outer.LocalDeltaStore(content + (a -> s), updates)
      // tracking updated bindings
      private def addUpdated(a: A) = this.copy(updates = updates + a)
      private def addUpdated(a: Iterable[A]) = this.copy(updates = updates ++ a)
      override def update(a: A, v: V) = super.update(a, v).addUpdated(a)
      override def collect(rs: Set[A]) = super.collect(rs).addUpdated(updates)
      override def join(other: outer.LocalDeltaStore): outer.LocalDeltaStore = super.join(other).addUpdated(other.updates)
  // d1 'after' d0
  // assumes that d1: sto1.DeltaStore, where sto1 = this.integrate(d0)
  def compose(d1: LocalStoreT[A, V]#LocalDeltaStore, d0: DeltaStore): DeltaStore =
    // assert(d1.parent == integrate(d0))
    LocalDeltaStore(d0.content ++ d1.content, d0.updates ++ d1.updates.filter(content.contains(_)))
  // replay changes of d
  // assumes that d: sto.DeltaStore, where sto = this.collect(rs) (for some rs)
  def replay(d: LocalStoreT[A, V]#LocalDeltaStore): DeltaStore =
    LocalDeltaStore(
      d.content.foldLeft(Map.empty[A, (V, Set[A], AbstractCount)]) { case (acc, (adr, s @ (v, r, c))) =>
        if d.parent.content.contains(adr) then {
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

case class LocalStore[A <: Address, V](
    content: Map[A, (V, Set[A], AbstractCount)]
  )(
    shouldCount: A => Boolean
  )(implicit val lattice: LatticeWithAddrs[V, A])
    extends LocalStoreT[A, V](shouldCount):
    type This = LocalStore[A, V]
    def empty = LocalStore(Map.empty)(shouldCount)
    def bind(a: A, s: (V, Set[A], AbstractCount)): LocalStore[A, V] = LocalStore(content + (a -> s))(shouldCount)

object LocalStore:
    def empty[A <: Address, V](shouldCount: A => Boolean)(implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A, V] =
      LocalStore(Map.empty)(shouldCount)
    def from[A <: Address, V](content: Iterable[(A, V)])(shouldCount: A => Boolean)(implicit lattice: LatticeWithAddrs[V, A]): LocalStore[A, V] =
      content.foldLeft(empty(shouldCount))((acc, bnd) => acc.extend(bnd._1, bnd._2))

*/