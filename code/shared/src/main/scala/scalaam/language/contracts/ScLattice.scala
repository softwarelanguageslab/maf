package scalaam.language.contracts

import scalaam.core.{Address, Environment, Identity, Lattice}
import scalaam.lattice.Type.Bottom
import scalaam.lattice.{BoolLattice, IntLattice}

/**
  * In this trait we define which operations a lattice for soft contracts
  * should implement.
  * @tparam L the type of the elements of the lattice
  */
trait ScLattice[L, Addr <: Address] extends Lattice[L] {

  /**
    * A representation for a primitive operation
    * @param operation the name of the operation represented a string
    */
  case class Prim(operation: String)

  /**
    * A class representing a closure,
    * a closure contains an environment, a list of formal parameters,
    * a location in source code and the body of the closure.
    */
  case class Clo(idn: Identity, env: Environment[Addr], parameters: List[ScIdentifier], body: ScExp)
  case class Grd(domain: Addr, rangeMaker: Addr)

  /**
    * An opaque value, we could add refinements to this value so that we can use those refinements
    * for further restriction of our state space
    */
  case class Opq(id: Int)

  /**
    * The value representing a blame
    *
    * @param lr: location of the contract, in case the contract itself is to blame for its violation
    * @param ld: location of the expression, in case the expression is to blame for the violation
    */
  case class Arr(lr: Identity, ld: Identity, contract: Addr, e: Addr)

  def injectBoolean(bool: Boolean): L
  def injectInteger(n: Int): L
  def injectClo(clo: Clo): L
  def injectGrd(grd: Grd): L
  def injectArr(arr: Arr): L
  def applyPrimitive(prim: Prim)(arguments: L*): L
  def isTrue(value: L): Boolean
  def isFalse(value: L): Boolean
}
