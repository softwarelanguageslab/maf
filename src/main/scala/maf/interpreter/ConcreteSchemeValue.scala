package maf.interpreter

import maf.util.*
import maf.values.typeclasses.*

sealed trait ConcreteSchemeValue
object ConcreteSchemeValue:
  given Conversion[Int, ConcreteSchemeValue] = SchemeInt.apply
  given Conversion[Double, ConcreteSchemeValue] = SchemeDouble.apply
  given Conversion[String, ConcreteSchemeValue] = SchemeString.apply
  given [L: GaloisFrom[ConcreteSchemeValue]]
      : Conversion[ConcreteSchemeValue, L] = Galois.inject

case class SchemeInt(int: Int) extends ConcreteSchemeValue
case class SchemeDouble(doub: Double) extends ConcreteSchemeValue
case class SchemeString(str: String) extends ConcreteSchemeValue
case class SchemePtr(adr: Address) extends ConcreteSchemeValue
case object SchemeNil extends ConcreteSchemeValue
