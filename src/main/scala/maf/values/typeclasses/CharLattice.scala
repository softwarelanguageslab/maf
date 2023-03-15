package maf.values
package typeclasses

type CharLattice_[I, Sym, S] = [C] =>> CharLattice[C, I, Sym, S]

/** A lattice for characters */
trait CharLattice[C, I: IntLattice, Sym: SymbolLattice, S: StringLattice_[
  I,
  C,
  Sym
]] extends Lattice[C]:
  def inject(c: Char): C
  def downCase(c: C): C
  def upCase(c: C): C
  def toInt[I: IntLattice](c: C): I
  def toString(c: C): S

  def isLower[B: BoolLattice](c: C): B
  def isUpper[B: BoolLattice](c: C): B

  def charEq[B: BoolLattice](c1: C, c2: C): B
  def charLt[B: BoolLattice](c1: C, c2: C): B

  def charEqCI[B: BoolLattice](c1: C, c2: C): B
  def charLtCI[B: BoolLattice](c1: C, c2: C): B

object CharLattice:
  def apply[I: IntLattice, Sym: SymbolLattice, S: StringLattice_[
    I,
    C,
    Sym
  ], C: CharLattice_[I, Sym, S]]: CharLattice[C, I, Sym, S] = summon
