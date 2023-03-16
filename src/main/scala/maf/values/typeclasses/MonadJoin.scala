package maf.values
package typeclasses

import cats.data.*
import cats.*
import cats.syntax.all._

/** A monad that supports joining two values of a lattice together */
trait MonadJoin[M[_]: Monad]:
  def mjoin[X: Lattice](a: M[X], b: M[X]): M[X]
  def mzero[X: Lattice]: M[X] = Lattice[X].bottom.pure
  def cond[X: Lattice, B: BoolLattice](b: B)(m1: M[X])(m2: M[X]): M[X] =
    val trueBranch = if BoolLattice[B].isTrue(b) then m1 else mzero[X]
    val falseBranch = if BoolLattice[B].isFalse(b) then m2 else mzero[X]
    mjoin(trueBranch, falseBranch)

object MonadJoin:
  def apply[M[_]: MonadJoin]: MonadJoin[M] = summon

package syntax {
  extension [M[_]: MonadJoin, X: Lattice](a: M[X])
    def ||(b: M[X]): M[X] =
      summon[MonadJoin[M]].mjoin(a, b)
}

/** A lattice operation is an operation that might fail */
type MonadLatOp[M[_]] = MonadError[M, Error] & MonadJoin[M]
