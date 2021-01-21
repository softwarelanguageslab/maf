package maf.modular.contracts

import maf.language.contracts.ScSchemeDomain
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.core.Address
import maf.lattice.ConstantPropagation
import maf.lattice.Concrete
import maf.language.scheme.primitives.SchemeLatticePrimitives

trait ScSchemeConstantPropagationDomain extends ScSchemeDomain[Address] {

  type S = ConstantPropagation.S
  type B = ConstantPropagation.B
  type I = ConstantPropagation.I
  type R = ConstantPropagation.R
  type C = ConstantPropagation.C
  type Sym = Concrete.Sym

  lazy val modularLattice: ModularSchemeLattice[Address, S, B, I, R, C, Sym] = new ModularSchemeLattice
  lazy val schemePrimitives = new SchemeLatticePrimitives()(schemeLattice)
}
