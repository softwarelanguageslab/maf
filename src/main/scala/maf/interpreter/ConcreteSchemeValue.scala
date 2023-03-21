package maf.interpreter

import maf.util.*
import maf.values.typeclasses.*

/** Hierarchie of concrete scheme values
  */
sealed trait ConcreteSchemeValue

/** Companion object that provides conversions from Scala values to Scheme
  * values, as well as injections into the abstract domain.
  */
object ConcreteSchemeValue:
  given Conversion[BigInt, ConcreteSchemeValue] = SchemeInt.apply
  given Conversion[Double, ConcreteSchemeValue] = SchemeDouble.apply
  given Conversion[String, ConcreteSchemeValue] = SchemeString.apply
  given Conversion[Boolean, ConcreteSchemeValue] = SchemeBoolean.apply

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
  class GaloisFor[A, L: GaloisFrom[ConcreteSchemeValue]](
      f: PartialFunction[ConcreteSchemeValue, A]
  )(using
      Conversion[A, ConcreteSchemeValue]
  ) extends Galois[A, L]:
    def inject(a: A): L = Galois.inject[ConcreteSchemeValue, L](a)
    def extract(v: L): Option[Set[A]] =
      summon[Galois[ConcreteSchemeValue, L]].extract(v).map(_.collect(f))

  given [L: GaloisFrom[ConcreteSchemeValue]]: Galois[Boolean, L] =
    GaloisFor[Boolean, L] { case SchemeBoolean(b) => b }
  given [L: GaloisFrom[ConcreteSchemeValue]]: Galois[BigInt, L] =
    GaloisFor[BigInt, L] { case SchemeInt(b) => b }
  given [L: GaloisFrom[ConcreteSchemeValue]]: Galois[String, L] =
    GaloisFor[String, L] { case SchemeString(b) => b }
  given [L: GaloisFrom[ConcreteSchemeValue]]: Galois[Double, L] =
    GaloisFor[Double, L] { case SchemeDouble(b) => b }

/** A Scheme integer
  * @note
  *   `BigInt` is used intentionally here to support the values that a typical
  *   R5RS implementation supports.
  */
case class SchemeInt(int: BigInt) extends ConcreteSchemeValue

/** Double precision floating point numbers.
  *
  * @note
  *   R5RS supports `inexact` and `exact` numbers, the former are usually
  *   represented by floating point numbers and the latter as fracitons. We do
  *   not support this currently.
  */
case class SchemeDouble(doub: Double) extends ConcreteSchemeValue

/** A Scheme String */
case class SchemeString(str: String) extends ConcreteSchemeValue

/** A pointer value wrapping a (concrete) address */
case class SchemePtr(adr: Address) extends ConcreteSchemeValue

/** A nil value */
case object SchemeNil extends ConcreteSchemeValue

/** An `unspecified` value, used for cetain operations such as `set!` */
case object SchemeUnspecified extends ConcreteSchemeValue

/** A boolean value */
case class SchemeBoolean(b: Boolean) extends ConcreteSchemeValue
