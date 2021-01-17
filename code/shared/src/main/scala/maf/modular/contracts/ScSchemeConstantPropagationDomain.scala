package maf.modular.contracts

import maf.language.contracts.ScSchemeDomain
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.core.Address
import maf.lattice.ConstantPropagation
import maf.lattice.Concrete
import maf.language.contracts.ScLattice

object ScSchemeConstantPropagationLattice extends ScSchemeDomain[Address] {

  type S = ConstantPropagation.S
  type B = ConstantPropagation.B
  type I = ConstantPropagation.I
  type R = ConstantPropagation.R
  type C = ConstantPropagation.C
  type Sym = Concrete.Sym

  lazy val modularLattice: ModularSchemeLattice[Address, S, B, I, R, C, Sym] = new ModularSchemeLattice
}

trait ScSchemeConstantPropagationDomain {
  implicit val lattice = ScSchemeConstantPropagationLattice.schemeLattice
}
