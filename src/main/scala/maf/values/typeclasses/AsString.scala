package maf.values.typeclasses

trait AsString[A]:
  def toString[Sym: SymbolLattice, I: IntLattice, C: CharLattice_[
    I,
    Sym,
    S
  ], S: StringLattice_[I, C, Sym]: GaloisFrom[String]](v: A): S