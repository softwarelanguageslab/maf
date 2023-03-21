package maf.values
package typeclasses

/** A maf.lattice for symbols */
trait SymbolLattice[Sym] extends Lattice[Sym]:
  def toString[I: IntLattice, C: CharLattice_[I, Sym, S], S: StringLattice_[
    I,
    C,
    Sym
  ]: GaloisFrom[String]](n: Sym): S

object SymbolLattice:
  def apply[Sym: SymbolLattice]: SymbolLattice[Sym] = implicitly
