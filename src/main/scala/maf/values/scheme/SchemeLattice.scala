package maf.values
package scheme

import maf.interpreter.ConcreteSchemeValue
import maf.util.*
import typeclasses.*
import maf.syntax.scheme.SchemeExp
import cats.extensions.*
import maf.interpreter.SimpleSchemeValue

given [L](using lat: SchemeLattice[L]): Galois[SimpleSchemeValue, L] with
  val galois = lat.galois
  export galois.*

trait Extractor[L, E]:
  def extract(v: L): Option[(E)]
  def unapply(v: L): Option[(E, L)] =
    extract(v).map((_, v))

object Extractor:
  def apply[L, E](f: PartialFunction[L, E]): Extractor[L, E] =
    (v: L) => f.andThen(Some(_)).applyOrElse(v, (_) => None)

/** Represents a lattice that supports all Scheme operations, that is: a
  * combination of the operations on strings, integers, booleans, real numbers,
  * symbols and characters.
  *
  * An implementation of this trait should represents the R5RS semantics for
  * these operations. For example, R5RS says that if a real number and an
  * integer number are added together, the result is a real number.
  *
  * The recommended representation of values in this lattice is a
  * `SparseProduct` which bundles several lattices together into a sparse
  * product lattice which has both efficient time and space characteristcs.
  *
  * @tparam L
  *   the type of the abstract Scheme value
  */
trait SchemeLattice[L]
    extends IntLattice[L],
      BoolLattice[L],
      RealLattice[L],
      SymbolLattice[L],
      CharLattice[L, L, L, L],
      VectorLattice[L, L, L],
      PairLattice[L, L],
      StringLattice[L, L, L, L],
      Lattice[L]:

  import ConcreteSchemeValue.given

  type Env = Unit // TODO

  /** A valid Scheme lattice should provide a Galois connection between concrete
    * and abstract values
    */
  given galois: Galois[SimpleSchemeValue, L]

  //
  // Extractors and predicates for types in the Scheme Lattice
  //

  /** Pattern for matching against singleton primitive values, singletons can be
    * obtained using method `split`
    */
  def primitive: Extractor[L, String]
  def isPrim[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A string, might contain a string corresponding to its concrete value */
  def isStr[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A boolean */
  def isBool[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A real number, might contain a concrete double */
  def isReal[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** An integer number */
  def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A symbol */
  def isSym[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A character */
  def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A pointer value, must contain an address */
  def isPtr[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A null values, does not contain any interesting subvalues */
  def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Unspecified values */
  def isUnsp[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Pattern for matching against singleton closures, which can be obtained
    * using method `split`
    */
  def closures: Extractor[L, (SchemeExp, Env)]
  def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Vector values */
  def isVec[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Pair values */
  def isPair[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Inject an address as a pointer in the abstract domain */
  def pointer(adr: Address): L

  /** Pattern for matching against singleton pointer values, which can be
    * obtained using method `split`
    */
  def ptr: Extractor[L, Address]

  /** Equality between two values */
  def eq(x: L, y: L)(comparePtr: MaybeEq[Address]): L

  // Convenience procedures
  def boolTop: L =
    join(Galois.inject[Boolean, L](true), Galois.inject[Boolean, L](false))

  // prevent name clashes between RealLattice and IntLattice
  override def isZero[B: BoolLattice: GaloisFrom[Boolean]](v: L)(using
      Galois[BigInt, L]
  ): B =
    eql(v, Galois.inject[SimpleSchemeValue, L](BigInt(0)))
