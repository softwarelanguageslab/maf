package maf.values
package scheme

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
      CharLattice[L, L, L, L]:

  // TODO: closures
  // TODO: primitives

  /** Inject the nil value in the abstract domain */
  def nil: L

  /** Inject a pair in the abstract domain */
  def cons(car: L, cdr: L): L

  /** Inject a address as a pointer in the abstract domain */
  def ptr(adr: Address): L

  // prevent name clashes between RealLattice and IntLattice
  override def isZero[B: BoolLattice](v: L): B =
    eql(v, inject(0))
