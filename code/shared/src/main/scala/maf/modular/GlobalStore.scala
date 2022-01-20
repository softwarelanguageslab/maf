package maf.modular

import maf.core._
import maf.util.StringUtil

// Dependency that is triggered when an abstract value at address 'addr' is updated
case class AddrDependency(addr: Address) extends Dependency:
    override def toString: String = s"AddrDep($addr)"

/**
 * Provides a global store to a modular analysis.
 * @tparam Expr
 *   The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] with AbstractDomain[Expr] { inter =>

  // TODO: should we parameterize this for more type-safety, or do we not care about that for addresses?
  type Addr = Address

  // parameterized by some store that can be accessed and modified
  def store: Map[Addr, Value]
  def store_=(store: Map[Addr, Value]): Unit

  def writeAddr(addr: Addr, value: Value): Boolean =
    updateAddr(inter.store, addr, value)
      .map(updated => inter.store = updated)
      .isDefined

  protected def updateAddr(
      store: Map[Addr, Value],
      addr: Addr,
      value: Value
    ): Option[Map[Addr, Value]] =
    store.get(addr) match
        case None if lattice.isBottom(value) => None
        case None                            => Some(store + (addr -> value))
        case Some(oldValue) =>
          val newValue = lattice.join(oldValue, value)
          if newValue == oldValue then None
          else Some(store + (addr -> newValue))

  override def intraAnalysis(cmp: Component): GlobalStoreIntra
  trait GlobalStoreIntra extends IntraAnalysis { intra =>
    // local copy of the global store
    var store = inter.store
    // reading addresses in the global store
    def readAddr(addr: Addr): Value =
        register(AddrDependency(addr))
        intra.store.getOrElse(addr, lattice.bottom)
    // writing addresses of the global store
    def writeAddr(addr: Addr, value: Value): Boolean =
      updateAddr(intra.store, addr, value).map { updated =>
          intra.store = updated
          trigger(AddrDependency(addr))
      }.isDefined

    override def doWrite(dep: Dependency): Boolean = dep match
        case AddrDependency(addr) => inter.writeAddr(addr, intra.store(addr))
        case _                    => super.doWrite(dep)
  }

  /** Returns a string representation of the store. */
  def storeString(primitives: Boolean = false): String =
      val strings = store.map({ case (a, v) => s"${StringUtil.toWidth(a.toString, 50)}: $v" })
      val filtered = if primitives then strings else strings.filterNot(_.toLowerCase.nn.startsWith("prm"))
      val size = filtered.size
      val infoString = "σ" * 150 + s"\nThe store contains $size addresses (primitive addresses ${if primitives then "included" else "excluded"}).\n"
      filtered.toList.sorted.mkString(infoString, "\n", "\n" + "σ" * 150)

  override def configString(): String = super.configString() + s"\n  with a global store and a $domainName"
}
