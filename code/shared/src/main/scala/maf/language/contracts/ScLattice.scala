package maf.language.contracts

import maf.core.{Address, Environment, Identity, Lattice}
import maf.lattice.interfaces.IntLattice

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
      pc: ScExp = ScNil(),
      topLevel: Boolean = false
  ) {

    /**
      * Captured variables are free variables that are captured from the enclosing environment, we denote them here
      * using their address in the store, such that we can evict them from the store cache when appropriate
      */
    val capturedVariables: List[Addr] = lambda.fv.flatMap(env.lookup).toList
  }

  /**
    * A guard which represents the value of a dependent contract after evaluation.
    * <code>
    *  (~> domain rangeMaker)
    * </code>
    */
  case class Grd[Addr](domain: List[Addr], rangeMaker: Addr)

  /**
    * An opaque value, we could add refinements to this value so that we can use those refinements
    * for further restriction of our state space
    */
  case class Opq(refinementSet: Set[String] = Set())

  /**
    * A monitor on a dependent contract
    *  (mon (~> domain rangeMaker)/lcontract procedure/lserver)
    */
  case class Arr[Addr](
      lcontract: Identity,
      lserver: Identity,
      contract: Addr,
      e: Addr,
      topLevel: Boolean = false
  )

  /**
    * A value that represents a flat contract, such that we can distribute blames correctly when a value
    * of this type is applied.
    */
  case class Flat[Addr](contract: Addr)

  /**
    * A blame that is generated when some contract has failed to be verified as safe.
    */
  case class Blame(blamedPosition: Identity, blamingPosition: Identity = Identity.none)

  /**
    * A mapping from a value (coming from a certain state) to a (possibly) refined opaque value
    */
  case class RefinedValueInState[+V](state: V, value: Opq)

  /**
    * A thunk is a parameterless lambda that simply returns the given value when
    * evaluated.
    *
    * (lambda () value)
    */
  case class Thunk[Addr <: Address](value: Addr)

  /**
    * A cons pair consisting of a car and a cdr
    */
  case class Cons[Addr <: Address](car: Addr, cdr: Addr)

  /**
    * Nil
    */
  case object Nil

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

  /**
    * Inject a thunk in the abstract domain
    */
  def injectThunk(thunk: Thunk[Addr]): L

  /**
    * Inject an opaque value from the given state in the abstract domain
    */
  def injectRefinedValueInState(state: L, value: Opq): L

  /**
    * Inject a cons value in the abstract domain
    */
  def injectCons(cons: Cons[Addr]): L

  /**
    * Inject nil in the abstract domain
    */
  def injectNil: L

  /**
    * Inject an address in the abstract domain
    */
  def injectPointer(a: Addr): L

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

  /**
    * Returns true if the value is possibly a thunk
    */
  def isThunk(value: L): Boolean

  /**
    * Returns true if the value is possibly a cons pair
    */
  def isCons(value: L): Boolean

  /**
    * Returns true if the value is possibly nil
    */
  def isNil(value: L): Boolean

  /**
    * Returns true if the value is possibly a vector
    */
  def isVec(value: L): Boolean

  /**
    * Returns true if the the value is a wrapped pointer
    */
  def isPointer(value: L): Boolean

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

  /**
    * Returns the symbolic representation of the value if available
    */
  def getSymbolic(value: L): Option[String]

  /**
    * Extract the set of opaque values associated with the given state
    */
  def getRefinedValueInState(state: L): Set[RefinedValueInState[L]]

  /**
    * Extracts the set of thunks from the abstract domain
    */
  def getThunk(value: L): Set[Thunk[Addr]]

  /**
    * Extracts the set of cons pairs from the abstract value
    */
  def getCons(value: L): Set[Cons[Addr]]

  /**
    * Extract the pointers contained within the value from the abstract domain.
    */
  def getPointers(value: L): Set[Addr]

  /*==================================================================================================================*/

  def integerTop: L
  def boolTop: L

  /**
    * Create a vector from a length represented as an abstract value
    * and with the default abstract value of `L`
    */
  def vector(length: L, init: L): L

  /**
    * Change the value of the vector `vector` on index `index` to value `value`
    */
  def vectorSet(vector: L, index: L, value: L): L

  /**
    * Retrieve a value on index `index` from  the vector
    */
  def vectorRef(vector: L, index: L): L
}
