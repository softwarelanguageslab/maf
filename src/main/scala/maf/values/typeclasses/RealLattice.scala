package maf.values
package typeclasses

import cats.extensions.*
import maf.util.Error

/** A lattice for reals (i.e., floating point numbers) */
trait RealLattice[R] extends Lattice[R] { self =>
  def toInt[M[_]: MonadError[Error]: MonadJoin, I: IntLattice: GaloisFrom[
    BigInt
  ]](n: R): M[I]
  def ceiling[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def floor[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def round[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def isZero[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice: GaloisFrom[
    Boolean
  ]](v: R)(using
      Galois[Double, R]
  ): B =
    eql(v, Galois.inject[Double, R](0))
  def log[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def random[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def sin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def asin[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def cos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def acos[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def tan[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def atan[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def sqrt[M[_]: MonadError[Error]: MonadJoin](n: R): M[R]
  def plus[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def minus[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def times[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def div[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def expt[M[_]: MonadError[Error]: MonadJoin](n1: R, n2: R): M[R]
  def lt[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice: GaloisFrom[
    Boolean
  ]](n1: R, n2: R): M[B]
  def toString[I: IntLattice, C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ]: GaloisFrom[String], Sym: SymbolLattice](n: R): S
}

object RealLattice:
  def apply[R: RealLattice]: RealLattice[R] = implicitly
