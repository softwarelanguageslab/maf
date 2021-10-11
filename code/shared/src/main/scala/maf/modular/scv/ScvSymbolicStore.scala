package maf.modular.scv

import maf.modular.{Dependency, ModAnalysis}
import maf.core.Expression
import maf.util.{Monoid, StringUtil}

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
  type A

  private var store: Map[A, V] = Map()
  override def intraAnalysis(cmp: Component): GlobalMapStoreIntra

  case class AddrMapDependency(addr: A) extends Dependency

  /** Forcefully write the `newvalue` to the specific `addr` in the store */
  private def writeAddr(addr: A, newvalue: V): Boolean =
    inter.store.get(addr) match
        case Some(oldvalue) if oldvalue == newvalue => false
        case _ =>
          store = store + (addr -> newvalue)
          true

  private def updateAddr[V: Monoid](
      store: Map[A, V],
      addr: A,
      value: V
    ): Option[Map[A, V]] =
    store.get(addr) match
        case None if Monoid[V].zero == value => None
        case None                            => Some(store + (addr -> value))
        case Some(oldValue) =>
          val newValue = Monoid[V].append(oldValue, value)
          if newValue == oldValue then None
          else Some(store + (addr -> newValue))

  trait GlobalMapStoreIntra extends IntraAnalysis { intra =>
    private var store = inter.store

    def writeMapAddr(addr: A, value: V): Unit =
      updateAddr[V](intra.store, addr, value).map { updated =>
          intra.store = updated
          trigger(AddrMapDependency(addr))
      }.isDefined

    def writeMapAddrForce(addr: A, value: V): Unit =
        intra.store += (addr -> value)
        trigger(AddrMapDependency(addr))

    def readMapAddr(addr: A): V =
        register(AddrMapDependency(addr))
        intra.store.getOrElse(addr, Monoid[V].zero)

    override def doWrite(dep: Dependency): Boolean = dep match
        case AddrMapDependency(addr) => inter.writeAddr(addr, intra.store(addr))
        case _                       => super.doWrite(dep)
  }

  def mapStoreString(primitives: Boolean = false): String =
      val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
      val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
      val size = filtered.size
      val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
      filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)
}

object ScvSymbolicStore:
    import maf.language.scheme._
    given Monoid[List[SchemeExp]] with
        def zero: List[SchemeExp] = List()
        def append(x: List[SchemeExp], y: => List[SchemeExp]): List[SchemeExp] = x ++ y

    trait GlobalSymbolicStore extends GlobalMapStore[List[SchemeExp], SchemeExp]:
        type A = Component
