package scalaam.modular.contracts

import scalaam.core.Environment
import scalaam.language.contracts.ScExp
import scalaam.language.contracts.ScLattice.Prim
import scalaam.modular.GlobalStore

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
      "dependent-contract?",
      "any?",
      "nonzero?"
    )
  def bindings     = primitives.map(p => (p, ScPrimAddr(p)))
  def baseEnv: Env = Environment(bindings)
  def setup: Unit = bindings.foreach {
    case (name, addr) => {
      store += addr -> lattice.injectPrim(Prim(name))
    }
  }
}
