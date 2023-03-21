package maf.values
package scheme

import maf.interpreter.ConcreteSchemeValue
import maf.util.*
import typeclasses.*

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
      CharLattice[L, L, L, L],
      Galois[ConcreteSchemeValue, L]:

  // TODO: closures
  // TODO: primitives

  def cons(car: L, cdr: L): L

  /** A valid Scheme lattice should provide a Galois connection between concrete
    * and abstract values
    */
  given galois: Galois[ConcreteSchemeValue, L]

  // prevent name clashes between RealLattice and IntLattice
  override def isZero[B: BoolLattice: GaloisFrom[Boolean]](v: L)(using
      Galois[BigInt, L]
  ): B =
    eql(v, Galois.inject[ConcreteSchemeValue, L](0))
