package maf.modular.contracts

import maf.core.Environment
import maf.language.contracts.ScExp
import maf.language.contracts.ScLattice.Prim
import maf.modular.GlobalStore

trait ScPrimitives extends ScModSemantics with GlobalStore[ScExp] {
  override var store: Map[Addr, Value] = Map()
  def primitives =
    List(
      "+",
      "-",
      "*",
      "/",
      "=",
      "int?",
      "proc?",
      ">",
      "<",
      "=<",
      "dependent-contract?",
      "any?",
      "and",
      "nonzero?",
      "not"
    )
  def primBindings = primitives.map(p => (p, ScPrimAddr(p)))
  def baseEnv: Env = Environment(primBindings)
  def setup(): Unit = primBindings.foreach {
    case (name, addr) => {
      store += addr -> lattice.injectPrim(Prim(name))
    }
  }
}
