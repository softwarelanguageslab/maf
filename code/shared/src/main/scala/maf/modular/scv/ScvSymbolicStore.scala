package maf.modular.scv

import maf.modular.{Dependency, ModAnalysis}
import maf.core.{Address, Expression}
import maf.util.Monoid

/**
 * A generic global store, which is not constrained on the type of values it can store, and is only concerned with values that can be appended
 * together (i.e., implement Monoid). This generic global store interface also features a way to forcefully update a particular value.
 *
 * @tparam V
 *   the type of values to store in the store
 * @tparam Exp
 *   the type of expression used in the analysis
 */
trait GlobalMapStore[V: Monoid, Exp <: Expression] extends ModAnalysis[Exp] { inter =>
  private var store: Map[Address, V] = Map()
  override def intraAnalysis(cmp: Component): GlobalMapStoreIntra

  case class AddrDependency(addr: Address) extends Dependency

  /** Forcefully write the `newvalue` to the specific `addr` in the store */
  private def writeAddr(addr: Address, newvalue: V): Boolean =
    inter.store.get(addr) match
        case Some(oldvalue) if oldvalue == newvalue => false
        case _ =>
          store = store + (addr -> newvalue)
          true

  private def updateAddr[V: Monoid](
      store: Map[Address, V],
      addr: Address,
      value: V
    ): Option[Map[Address, V]] =
    store.get(addr) match
        case None if Monoid[V].zero == value => None
        case None                            => Some(store + (addr -> value))
        case Some(oldValue) =>
          val newValue = Monoid[V].append(oldValue, value)
          if newValue == oldValue then None
          else Some(store + (addr -> newValue))

  trait GlobalMapStoreIntra extends IntraAnalysis { intra =>
    private var store = inter.store

    def writeMapAddr(addr: Address, value: V): Unit =
      updateAddr[V](intra.store, addr, value).map { updated =>
          intra.store = updated
          trigger(AddrDependency(addr))
      }.isDefined

    def readMapAddr(addr: Address): V =
        register(AddrDependency(addr))
        intra.store.getOrElse(addr, Monoid[V].zero)

    override def doWrite(dep: Dependency): Boolean = dep match
        case AddrDependency(addr) => inter.writeAddr(addr, intra.store(addr))
        case _                    => super.doWrite(dep)
  }
}

object ScvSymbolicStore:
    import maf.language.scheme._
    given Monoid[List[SchemeExp]] with
        def zero: List[SchemeExp] = List()
        def append(x: List[SchemeExp], y: => List[SchemeExp]): List[SchemeExp] = x ++ y

    trait GlobalSymbolicStore extends GlobalMapStore[List[SchemeExp], SchemeExp]
