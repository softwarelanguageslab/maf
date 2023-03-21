package maf.values
package typeclasses

/** A lattice for booleans */
trait BoolLattice[B] extends Lattice[B]:
  def isTrue(b: B): Boolean
  def isFalse(b: B): Boolean
  def not(b: B): B
  def top: B

object BoolLattice:
  def apply[B: BoolLattice]: BoolLattice[B] = implicitly
