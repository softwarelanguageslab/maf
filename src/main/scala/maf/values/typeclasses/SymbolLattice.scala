package maf.values
package typeclasses

/** A maf.lattice for symbols */
trait SymbolLattice[Sym] extends Lattice[Sym], AsString[Sym]
object SymbolLattice:
  def apply[Sym: SymbolLattice]: SymbolLattice[Sym] = implicitly
