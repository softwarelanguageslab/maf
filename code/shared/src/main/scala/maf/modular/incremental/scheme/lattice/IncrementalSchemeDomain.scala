package maf.modular.incremental.scheme.lattice

import maf.core._
import maf.language.scheme.SchemeExp
import maf.language.scheme.lattices.SchemeLattice
import maf.language.scheme.primitives.{SchemeLatticePrimitives, SchemePrimitives}
import maf.lattice.{ConstantPropagation, Type}
import maf.modular.AbstractDomain
import maf.modular.scheme._

// TODO: put declarations in correct place (wrapper).
trait IncrementalAbstractDomain[Expr <: Expression] extends AbstractDomain[Expr] {
  implicit override val lattice: IncrementalLattice[Value, Address]
  def addAddress(v: Value, source: Address): Value = addAddresses(v, Set(source))
  def addAddresses(v: Value, sources: Set[Address]): Value
  def removeAddresses(v: Value): Value
}

trait IncrementalSchemeDomain extends IncrementalAbstractDomain[SchemeExp] with SchemeDomain {
  implicit override val lattice: SchemeLattice[Value, Address] with IncrementalLattice[Value, Address]
}

trait IncrementalModularSchemeDomain extends IncrementalSchemeDomain {
  val modularLatticeWrapper: IncrementalModularSchemeLatticeWrapper
  type Value = modularLatticeWrapper.modularLattice.AL
  lazy val lattice = modularLatticeWrapper.modularLattice.incrementalSchemeLattice
  lazy val primitives = modularLatticeWrapper.primitives
  def addAddresses(v: modularLatticeWrapper.modularLattice.AL, sources: Set[Address]): modularLatticeWrapper.modularLattice.AL =
    lattice.addAddresses(v, sources)
  def removeAddresses(v: modularLatticeWrapper.modularLattice.AL): modularLatticeWrapper.modularLattice.AL = lattice.removeAddresses(v)
}

trait IncrementalModularSchemeLatticeWrapper {
  type S
  type B
  type I
  type R
  type C
  type Sym
  val modularLattice: IncrementalModularSchemeLattice[Address, S, B, I, R, C, Sym]
  val primitives: SchemePrimitives[modularLattice.AL, Address]
  def addAddresses(v: modularLattice.AL, sources: Set[Address]): modularLattice.AL = modularLattice.incrementalSchemeLattice.addAddresses(v, sources)
  def getAddresses(v: modularLattice.AL): Set[Address] = modularLattice.incrementalSchemeLattice.getAddresses(v)
  def removeAddresses(v: modularLattice.AL): modularLattice.AL = modularLattice.incrementalSchemeLattice.removeAddresses(v)
}

object IncrementalSchemeTypeDomain extends IncrementalModularSchemeLatticeWrapper {
  type S = Type.S
  type B = ConstantPropagation.B
  type I = Type.I
  type R = Type.R
  type C = Type.C
  type Sym = Type.Sym
  lazy val modularLattice = new IncrementalModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)
}

trait IncrementalSchemeTypeDomain extends IncrementalModularSchemeDomain {
  lazy val modularLatticeWrapper = IncrementalSchemeTypeDomain
}

object IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeLatticeWrapper {
  type S = ConstantPropagation.S
  type B = ConstantPropagation.B
  type I = ConstantPropagation.I
  type R = ConstantPropagation.R
  type C = ConstantPropagation.C
  type Sym = ConstantPropagation.Sym
  lazy val modularLattice = new IncrementalModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)
}

trait IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeDomain {
  lazy val modularLatticeWrapper = IncrementalSchemeConstantPropagationDomain
}
