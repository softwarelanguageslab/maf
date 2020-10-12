package maf.language.contracts

import maf.core.{Address, Environment, Identity, Lattice}

object ScLattice {

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
  case class Clo[Addr <: Address](
      idn: Identity,
      env: Environment[Addr],
      parameters: List[ScIdentifier],
      lambda: ScLambda,
      pc: ScExp = ScNil()
  )

  /**
    * A guard which represents the value of a dependent contract after evaluation.
    * <code>
    *  (domain ~ rangeMaker)
    * </code>
    */
  case class Grd[Addr](domain: Addr, rangeMaker: Addr)

  /**
    * An opaque value, we could add refinements to this value so that we can use those refinements
    * for further restriction of our state space
    */
  case class Opq(refinementSet: Set[String] = Set())

  /**
    * A monitor on a dependent contract
    * <code>
    *  (mon (domain ~ rangeMaker)/lcontract procedure/lserver)
    */
  case class Arr[Addr](lcontract: Identity, lserver: Identity, contract: Addr, e: Addr)

  /**
    * A value that represents a flat contract, such that we can distribute blames correctly when a value
    * of this type is applied.
    */
  case class Flat[Addr](contract: Addr)

  /**
    * A blame that is generated when some contract has failed to be verified as safe.
    */
  case class Blame(blamedPosition: Identity, blamingPosition: Identity = Identity.none)

  case class Symbolic(expr: ScExp)
}

/**
  * In this trait we define which operations a lattice for soft contracts
  * should implement.
  * @tparam L the type of the elements of the lattice
  */
trait ScLattice[L, Addr <: Address] extends Lattice[L] {
  import ScLattice._

  /*==================================================================================================================*/

  /**
    * Inject a primitive in the abstract domain
    */
  def injectPrim(prim: Prim): L

  /**
    * Inject a boolean in the abstract domain
    */
  def injectBoolean(bool: Boolean): L

  /**
    * Inject an integer in the abstract domain
    */
  def injectInteger(n: Int): L

  /**
    * Inject a clojure in the abstract domain
    */
  def injectClo(clo: Clo[Addr]): L

  /**
    * Inject a guard in the abstract domain
    */
  def injectGrd(grd: Grd[Addr]): L

  /**
    * Inject an arrow (monitors on functions) in the abstract domain
    */
  def injectArr(arr: Arr[Addr]): L

  def injectSymbolic(sym: Symbolic): L

  /**
    * Inject a blame in the abstract domain
    */
  def injectBlame(blame: Blame): L

  /**
    * Inject an opaque value in the abstract domain
    */
  def injectOpq(opq: Opq): L

  /**
    * Inject a flat contract in the abstract domain
    */
  def injectFlat(flat: Flat[Addr]): L

  /*==================================================================================================================*/

  def applyPrimitive(prim: Prim)(arguments: L*): L

  /*==================================================================================================================*/

  /**
    * Returns true when the abstract value is possible true
    */
  def isTrue(value: L): Boolean

  /**
    * Returns true when the value is possibly false
    */
  def isFalse(value: L): Boolean

  /**
    * Returns true when the value is possibly a primitive
    */
  def isPrim(value: L): Boolean

  /**
    * Returns true if the value is possibly a closure
    */
  def isClo(value: L): Boolean

  /**
    * Returns true if the value is possible a blame
    */
  def isBlame(value: L): Boolean

  /**
    * Returns true if the abstract value is definitely an opaque value
    * @param value
    * @return
    */
  def isDefinitelyOpq(value: L): Boolean

  /**
    * Returns true if the value is possibly a flat contract
    */
  def isFlatContract(value: L): Boolean

  /*==================================================================================================================*/

  /**
    * Extract a set of primitives from the abstract value
    */
  def getPrim(value: L): Set[Prim]

  /**
    * Extract a set of arrow (monitors on functions) from the abstract value
    */
  def getArr(value: L): Set[Arr[Addr]]

  /**
    * Extract a set of closures from the abstract value
    */
  def getClo(value: L): Set[Clo[Addr]]

  /**
    * Extract a set of guards from the abstract value
    */
  def getGrd(value: L): Set[Grd[Addr]]

  /**
    * Extract a set of blames from the abstract value
    */
  def getBlames(value: L): Set[Blame]

  /**
    * Extract a set of opaque values from the abstract value
    */
  def getOpq(value: L): Set[Opq]

  /**
    * Extract the set of flat contracts from the abstract value
    */
  def getFlat(value: L): Set[Flat[Addr]]

  /*==================================================================================================================*/

  def integerTop: L
  def boolTop: L
}
