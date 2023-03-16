package maf.values
package scheme

import typeclasses.*

trait SchemeLattice[L]
    extends IntLattice[L],
      StringLattice[L, L, L, L],
      BoolLattice[L],
      RealLattice[L],
      SymbolLattice[L],
      CharLattice[L, L, L, L]:

  // prevent name clashes between RealLattice and IntLattice
  override def isZero[B: BoolLattice](v: L): B =
    eql(v, inject(0))
