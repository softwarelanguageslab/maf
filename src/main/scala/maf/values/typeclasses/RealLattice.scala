package maf.values
package typeclasses

import cats.extensions.*
import maf.util.Error

/** A lattice for reals (i.e., floating point numbers) */
trait RealLattice[R] extends Lattice[R] { self =>
  def inject(n: Double): R
  def toInt[I: IntLattice](n: R): I
  def ceiling(n: R): R
  def floor(n: R): R
  def round(n: R): R
  def isZero[B: BoolLattice](v: R): B =
    eql(v, inject(0))
  def log[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def random(n: R): R
  def sin(n: R): R
  def asin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def cos(n: R): R
  def acos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def tan(n: R): R
  def atan(n: R): R
  def sqrt[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def plus(n1: R, n2: R): R
  def minus(n1: R, n2: R): R
  def times(n1: R, n2: R): R
  def div[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def expt(n1: R, n2: R): R
  def lt[B: BoolLattice](n1: R, n2: R): B
  def toString[I: IntLattice, C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ], Sym: SymbolLattice](n: R): S
}

object RealLattice:
  def apply[R: RealLattice]: RealLattice[R] = implicitly
