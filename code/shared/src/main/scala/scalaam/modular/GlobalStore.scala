package scalaam.modular

import scalaam.core._

sealed trait A[+Component] extends Address
case class GlobalAddr(addr: Address) extends A[Nothing]                                 { def printable = addr.printable }
case class ComponentAddr[Component](cmp: Component, addr: Address) extends A[Component] { def printable = addr.printable } 

/**
 * An analysis with a global store.
 * @tparam Expr The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] { inter =>

  // parameterized by the type of address and abstract values
  type Addr <: Address
  type Value
  implicit val lattice: Lattice[Value]

  // parameterized by some store that can be accessed and modified
  var store: Map[Addr, Value]

  // parameterized by how addresses are allocated
  def sharedAddr(addr: Address): Addr
  def componentAddr(cmp: Component, addr: Address): Addr 

  // Dependency that is triggered when an abstract value at address 'addr' is updated
  case class AddrDependency(addr: Addr) extends Dependency {
    override def toString(): String = s"$addr"
  }

  private def updateAddr(store: Map[Addr,Value], addr: Addr, value: Value): Option[Map[Addr,Value]] = 
    store.get(addr) match {
      case None if value == lattice.bottom => None
      case None => Some(store + (addr -> value))
      case Some(oldValue) =>
        val newValue = lattice.join(oldValue, value)
        if (newValue == oldValue) {
          None
        } else {
          Some(store + (addr -> newValue))
        }
    }

  trait GlobalStoreIntra extends super.IntraAnalysis { intra => 
    // local copy of the global store
    var store = inter.store
    // allocating an address
    def allocAddr(addr: Address): Addr =
      componentAddr(component, addr)
    // reading addresses in the global store
    def readAddr(addr: Addr): Value = {
      register(AddrDependency(addr))
      intra.store.get(addr) match {
        case None => 
          intra.store = intra.store + (addr -> lattice.bottom) // TODO: <- currently required by AdaptiveGlobalStore, but can go once fixed there
          return lattice.bottom
        case Some(v) => 
          return v
      }
    }
    // writing addresses of the global store
    def writeAddr(addr: Addr, value: Value): Boolean = 
      updateAddr(intra.store, addr, value)
        .map(updated => {
          intra.store = updated
          trigger(AddrDependency(addr))
        })
        .isDefined

    override def commit(dep: Dependency): Boolean = dep match {
      case AddrDependency(addr) => 
        updateAddr(inter.store, addr, intra.store(addr))
          .map(updated => inter.store = updated)
          .isDefined
      case _ => super.commit(dep)
    }
  }
}

trait DedicatedGlobalStore[Expr <: Expression] extends GlobalStore[Expr] { inter => 
  // a store is just a fresh and empty map
  var store: Map[Addr,Value] = Map.empty
  // addresses are component addresses
  type Addr = A[Component]
  // allocating addresses
  def componentAddr(cmp: Component, addr: Address) = ComponentAddr(cmp, addr)
  def sharedAddr(addr: Address) = GlobalAddr(addr)
}