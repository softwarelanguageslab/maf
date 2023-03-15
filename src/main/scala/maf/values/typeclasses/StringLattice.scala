package maf.values
package typeclasses

import cats.extensions.*

case object NotANumberString extends Error

type StringLattice_[I, C, Sym] = [S] =>> StringLattice[S, I, C, Sym]

/** A lattice for strings */
trait StringLattice[S, I: IntLattice, C: CharLattice_[
  I,
  Sym,
  S
], Sym: SymbolLattice]
    extends Lattice[S]:

  def inject(s: String): S
  def length(s: S): I
  def append(s1: S, s2: S): S
  def substring[M[_]: MonadError[Error]](
      s: S,
      from: I,
      to: I
  ): M[S]
  def ref[M[_]: MonadError[Error]](s: S, i: I): M[C]
  def set[M[_]: MonadError[Error]](
      s: S,
      i: I,
      c: C
  ): M[S]

  def lt[B: BoolLattice](s1: S, s2: S): B
  def toSymbol(s: S): Sym
  def toNumber[M[_]: MonadError[Error]](s: S): M[I]
  def makeString(length: I, char: C): S

object StringLattice:
  def apply[I: IntLattice, C: CharLattice_[I, Sym, S], Sym: SymbolLattice, S](
      using StringLattice[S, I, C, Sym]
  ): StringLattice[S, I, C, Sym] =
    summon
