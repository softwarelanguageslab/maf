package maf.interpreter

import maf.util.*
import maf.values.typeclasses.*

/** Hierarchie of concrete scheme values
  */
sealed trait ConcreteSchemeValue
sealed trait SimpleSchemeValue extends ConcreteSchemeValue
sealed trait ConcreteSchemePtr extends ConcreteSchemeValue

/** Companion object that provides conversions from Scala values to Scheme
  * values, as well as injections into the abstract domain.
  */
object SimpleSchemeValue:
  given Conversion[BigInt, SimpleSchemeValue] = SchemeInt.apply
  given Conversion[Double, SimpleSchemeValue] = SchemeDouble.apply
  given Conversion[String, SimpleSchemeValue] = SchemeString.apply
  given Conversion[Boolean, SimpleSchemeValue] = SchemeBoolean.apply
  given Conversion[Address, SimpleSchemeValue] = SchemePtr.apply
  given Conversion[Char, SimpleSchemeValue] = SchemeChar.apply

  //
  // Galois
  //
  given [L: GaloisFrom[ConcreteSchemeValue]]
      : Conversion[ConcreteSchemeValue, L] = Galois.inject

  // Galois for each value

  /** Provides a Galois implementation for a value contained within a
    * `ConcreteSchemeValue`
    *
    * @param f
    *   a function that maps a `ConcreteSchemeValue` to its desired constituent
    * @tparam A
    *   the type of the consituent
    * @tparam L
    *   the type of the abstract value, can by anything as its domain supports
    *   injecting scheme values.
    */
  class GaloisForSubdomain[A, L: GaloisFrom[SimpleSchemeValue]](
      f: PartialFunction[SimpleSchemeValue, A]
  )(using
      conv: Conversion[A, SimpleSchemeValue]
  ) extends Galois[A, L]:
    def inject(a: A): L = Galois.inject[SimpleSchemeValue, L](conv(a))

  given [L: GaloisFrom[SimpleSchemeValue]]: Galois[Boolean, L] =
    GaloisForSubdomain[Boolean, L] { case SchemeBoolean(b) => b }
  given [L: GaloisFrom[SimpleSchemeValue]]: Galois[BigInt, L] =
    GaloisForSubdomain[BigInt, L] { case SchemeInt(b) => b }
  given [L: GaloisFrom[SimpleSchemeValue]]: Galois[String, L] =
    GaloisForSubdomain[String, L] { case SchemeString(b) => b }
  given [L: GaloisFrom[SimpleSchemeValue]]: Galois[Double, L] =
    GaloisForSubdomain[Double, L] { case SchemeDouble(b) => b }
  given [L: GaloisFrom[SimpleSchemeValue]]: Galois[Char, L] =
    GaloisForSubdomain[Char, L] { case SchemeChar(c) => c }

object ConcreteSchemeValue:
  export SimpleSchemeValue.{given, *}

/** A Scheme integer
  * @note
  *   `BigInt` is used intentionally here to support the values that a typical
  *   R5RS implementation supports.
  */
case class SchemeInt(int: BigInt) extends SimpleSchemeValue

/** Double precision floating point numbers.
  *
  * @note
  *   R5RS supports `inexact` and `exact` numbers, the former are usually
  *   represented by floating point numbers and the latter as fracitons. We do
  *   not support this currently.
  */
case class SchemeDouble(doub: Double) extends SimpleSchemeValue

/** A Scheme String */
case class SchemeString(str: String) extends SimpleSchemeValue

/** A pointer value wrapping a (concrete) address */
case class SchemePtr(adr: Address) extends SimpleSchemeValue
case class SchemeConsPtr(adr: Address) extends ConcreteSchemeValue
case class SchemeVecPtr(adr: Address) extends ConcreteSchemeValue
case class SchemeStrPtr(adr: Address) extends ConcreteSchemeValue

/** A nil value */
case object SchemeNil extends SimpleSchemeValue

/** An `unspecified` value, used for cetain operations such as `set!` */
case object SchemeUnspecified extends SimpleSchemeValue

/** A boolean value */
case class SchemeBoolean(b: Boolean) extends SimpleSchemeValue

/** A Scheme character value */
case class SchemeChar(c: Char) extends SimpleSchemeValue
