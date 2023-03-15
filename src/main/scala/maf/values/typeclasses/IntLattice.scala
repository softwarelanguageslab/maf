package maf.values
package typeclasses

/** A lattice for integers */
trait IntLattice[I] extends Lattice[I] { self =>
  def inject(n: BigInt): I

  def toReal[R: RealLattice](n: I): R
  def random(n: I): I
  def plus(n1: I, n2: I): I
  def minus(n1: I, n2: I): I
  def times(n1: I, n2: I): I
  def quotient(n1: I, n2: I): I
  def div[R: RealLattice](n1: I, n2: I): R
  def expt(n1: I, n2: I): I
  def modulo(n1: I, n2: I): I
  def remainder(n1: I, n2: I): I
  def lt[B: BoolLattice](n1: I, n2: I): B
  def valuesBetween(n1: I, n2: I): Set[I]
  def toString[C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ], Sym: SymbolLattice](n: I): S
  def toChar[C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ], Sym: SymbolLattice](n: I): C
}

object IntLattice:
  def apply[I: IntLattice]: IntLattice[I] = implicitly
