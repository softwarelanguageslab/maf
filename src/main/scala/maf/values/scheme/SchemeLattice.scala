package maf.values
package scheme

import maf.interpreter.ConcreteSchemeValue
import maf.util.*
import typeclasses.*
import maf.syntax.scheme.SchemeExp
import cats.extensions.*

given [L](using lat: SchemeLattice[L]): Galois[ConcreteSchemeValue, L] with
  val gal = lat.galois
  export gal.*

/** Represents a lattice that supports all Scheme operations, that is: a
  * combination of the operations on strings, integers, booleans, real numbers,
  * symbols and characters.
  *
  * An implementation of this trait should represents the R5RS semantics for
  * these operations. For example, R5RS says that if a real number and an
  * integer number are added together, the result is a real number.
  *
  * The recommended representation of values in this lattice is an `HMap` which
  * bundles several lattices together into a sparse product lattice which has
  * both efficient time and space characteristcs.
  */
trait SchemeLattice[L]
    extends IntLattice[L],
      StringLattice[L, L, L, L],
      BoolLattice[L],
      RealLattice[L],
      SymbolLattice[L],
      CharLattice[L, L, L, L]:

  import ConcreteSchemeValue.given

  type Env = Unit // TODO

  /** A valid Scheme lattice should provide a Galois connection between concrete
    * and abstract values
    */
  given galois: Galois[ConcreteSchemeValue, L]

  //
  // Types in the Scheme Lattice
  //
  // This hierarchy provides an abstract notion of the values contained within a Scheme value.
  // To interact with the parts of an abstract Scheme value, two methods are provided: `elements` and `is`.
  //
  // The former returns a list of elements in the abstract Scheme value, while the latter checks whether the Scheme value contains the given type.
  //

  /** A canonical represention of Scheme values in this lattice */
  type CanonicalValue = KeyPair[SchemeType]

  /** A hierarchy of Scheme types */
  protected trait SchemeType extends Key:
    type Concrete
    type Abstract

    val name: String

  /** Auxilary trait to easily define type tags, alias for SchemeType filling
    * the abstract type member `Value` using type parameter E
    *
    * @note
    *   in addition to the expected extracted type, the `unapply` method also
    *   returns an untyped version of the contained value, this can be used for
    *   error reporting.
    */
  trait SchemeTypeAux[Can](val name: String)
      extends SchemeType,
        Key.Aux[(Can, Any)]

  /** Scheme primitives, must at least have a `String` field corresponding to
    * the name of the primitive
    */
  case object CPrimT extends SchemeTypeAux[String]("primitive?")
  def isPrim[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A string, might contain a string corresponding to its concrete value */
  case object CStrT extends SchemeTypeAux[Option[String]]("string?")
  def isStr[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A boolean, might contain a concrete boolean */
  case object CBoolT extends SchemeTypeAux[Option[Boolean]]("boolean?")
  def isBool[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A real number, might contain a concrete double */
  case object CRealT extends SchemeTypeAux[Option[Double]]("real?")
  def isReal[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** An integer number */
  case object CIntT extends SchemeTypeAux[Unit]("integer?")
  def isInt[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A symbol */
  case object CSymT extends SchemeTypeAux[Unit]("symbol?")
  def isSym[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A character */
  case object CCharT extends SchemeTypeAux[Unit]("char?")
  def isChar[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A pointer value, must contain an address */
  case object CPtrT extends SchemeTypeAux[Address]("pointer?")
  def isPtr[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A pair, may contain a car and a cdr */
  case object CPaiT extends SchemeTypeAux[(Option[L], Option[L])]("pair?")
  def isPai[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** A null values, does not contain any interesting subvalues */
  case object CNullT extends SchemeTypeAux[Unit]("null?")
  def isNull[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Closure values */
  case object CCCloT extends SchemeTypeAux[(SchemeExp, Env)]("closure?")
  def isClo[B: BoolLattice: GaloisFrom[Boolean]](v: L): B

  /** Retrieve the elements contained within the abstract value.
    *
    * An implementation of this method must return all values of the
    * `SchemeType` hiearchy.
    */
  def elements(v: L): List[CanonicalValue]

  // TODO: closures
  // TODO: primitives

  def cons(car: L, cdr: L): L
  def car[M[_]: MonadError[Error]: MonadJoin](v: L): M[L]
  def cdr[M[_]: MonadError[Error]: MonadJoin](v: L): M[L]

  def pointer(adr: Address): L
  def vector[M[_]: MonadError[Error]: MonadJoin](siz: L, init: L): M[L]

  // Convenience procedures
  def boolTop: L =
    join(Galois.inject[Boolean, L](true), Galois.inject[Boolean, L](false))

  // prevent name clashes between RealLattice and IntLattice
  override def isZero[B: BoolLattice: GaloisFrom[Boolean]](v: L)(using
      Galois[BigInt, L]
  ): B =
    eql(v, Galois.inject[ConcreteSchemeValue, L](BigInt(0)))
