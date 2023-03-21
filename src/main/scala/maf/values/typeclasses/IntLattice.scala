package maf.values
package typeclasses

import maf.util.Error
import cats.extensions.*

/** A lattice for integers */
trait IntLattice[I] extends Lattice[I] { self =>
  def toReal[M[_]: MonadError[Error]: MonadJoin, R: RealLattice: GaloisFrom[
    Double
  ]](n: I): M[R]
  def random[M[_]: MonadError[Error]: MonadJoin](n: I): M[I]
  def plus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def minus[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def times[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def quotient[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def isZero[B: BoolLattice: GaloisFrom[Boolean]](v: I)(using
      Galois[BigInt, I]
  ): B =
    eql(v, Galois.inject[BigInt, I](0))

  def div[M[_], R: GaloisFrom[Double]](
      n1: I,
      n2: I
  )(using
      e1: cats.MonadError[M, Error],
      e2: maf.values.typeclasses.MonadJoin[M],
      e3: maf.values.typeclasses.RealLattice[R]
  ): M[R]

  def expt[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def modulo[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def remainder[M[_]: MonadError[Error]: MonadJoin](n1: I, n2: I): M[I]
  def lt[M[_]: MonadError[Error]: MonadJoin, B: BoolLattice: GaloisFrom[
    Boolean
  ]](n1: I, n2: I): M[B]
  def valuesBetween(n1: I, n2: I): Set[I]
  def toString[C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ]: GaloisFrom[String], Sym: SymbolLattice](n: I): S
  def toChar[C: CharLattice_[I, Sym, S]: GaloisFrom[Char], S: StringLattice_[
    I,
    C,
    Sym
  ], Sym: SymbolLattice](n: I): C
}

object IntLattice:
  def apply[I: IntLattice]: IntLattice[I] = implicitly
