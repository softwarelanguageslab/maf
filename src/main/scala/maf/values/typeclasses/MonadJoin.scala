package maf.values
package typeclasses

import cats.data.*
import cats.*
import cats.syntax.all._

/** A monad that supports joining two values of a lattice together */
trait MonadJoin[M[_]: Monad]:
    /** Joins two computations resulting in a value from a lattice together. The operation is supposed to behave as a `join` from a latticE. The
      * following properties should hold:
      *   - mjoin(mzero, a) = a /\ mjoin(a, mzero) = a
      *   - mjoin(a, b) = mjoin(b, a)
      *   - mjoin(a, mjoin(b, c)) = mjoin(mjoin(a, b), c)
      */
    def mjoin[X: Lattice](a: M[X], b: M[X]): M[X]
    def mjoin[X: Lattice](xs: Iterable[M[X]]): M[X] =
        xs.foldLeft(mzero: M[X])((acc, m) => mjoin(acc, m))

    /** The absorbing ("zero") computation */
    def mzero[X: Lattice]: M[X] = Lattice[X].bottom.pure

    def mfoldMap[X, Y: Lattice](xs: Iterable[X])(f: X => M[Y]): M[Y] =
        mjoin(xs.map(f))
    def cond[X: Lattice, B: BoolLattice](b: B)(m1: M[X])(m2: M[X]): M[X] =
        val trueBranch = if BoolLattice[B].isTrue(b) then m1 else mzero[X]
        val falseBranch = if BoolLattice[B].isFalse(b) then m2 else mzero[X]
        mjoin(trueBranch, falseBranch)

    def guard(b: Boolean)(e: => M[Unit]): M[Unit] =
        if b then ().pure else e

    // alternative version of `cond` that expresses the boolean condition as a monad
    def condM[X: Lattice, B: BoolLattice](mb: M[B])(m1: M[X])(m2: M[X]): M[X] =
        val trueBranch =
            mb >>= (b => if BoolLattice[B].isTrue(b) then m1 else mzero[X])
        val falseBranch =
            mb >>= (b => if BoolLattice[B].isFalse(b) then m2 else mzero[X])
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
