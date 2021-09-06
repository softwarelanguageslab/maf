package maf.modular.incremental.scheme.lattice

import maf.core._
import maf.language.scheme.SchemeExp
import maf.language.scheme.primitives._
import maf.lattice._
import maf.modular.scheme._

trait IncrementalSchemeDomain extends IncrementalAbstractDomain[SchemeExp] with SchemeDomain:
  implicit override lazy val lattice: IncrementalSchemeLattice[Value, Address]

trait IncrementalModularSchemeDomain extends IncrementalSchemeDomain:
  val modularLatticeWrapper: IncrementalModularSchemeLatticeWrapper
  type Value = modularLatticeWrapper.modularLattice.AL // Use the annotated lattice.
  override lazy val lattice = modularLatticeWrapper.modularLattice.incrementalSchemeLattice
  override lazy val primitives = modularLatticeWrapper.primitives
  def addAddresses(v: modularLatticeWrapper.modularLattice.AL, sources: Set[Address]): modularLatticeWrapper.modularLattice.AL =
    lattice.addAddresses(v, sources)
  def getAddresses(v: modularLatticeWrapper.modularLattice.AL): Set[Address] = lattice.getAddresses(v)
  def removeAddresses(v: modularLatticeWrapper.modularLattice.AL): modularLatticeWrapper.modularLattice.AL = lattice.removeAddresses(v)

trait IncrementalModularSchemeLatticeWrapper:
  type S
  type B
  type I
  type R
  type C
  type Sym
  // Contains the incremental modular Scheme lattice.
  val modularLattice: IncrementalModularSchemeLattice[Address, S, B, I, R, C, Sym]
  // Primitive functions of Scheme.
  val primitives: SchemePrimitives[modularLattice.AL, Address]

object IncrementalSchemeTypeDomain extends IncrementalModularSchemeLatticeWrapper:
  type S = Type.S
  type B = ConstantPropagation.B
  type I = Type.I
  type R = Type.R
  type C = Type.C
  type Sym = Type.Sym
  val modularLattice = new IncrementalModularSchemeLattice
  val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)

trait IncrementalSchemeTypeDomain extends IncrementalModularSchemeDomain:
  val modularLatticeWrapper = IncrementalSchemeTypeDomain

object IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeLatticeWrapper:
  type S = ConstantPropagation.S
  type B = ConstantPropagation.B
  type I = ConstantPropagation.I
  type R = ConstantPropagation.R
  type C = ConstantPropagation.C
  type Sym = ConstantPropagation.Sym
  val modularLattice = new IncrementalModularSchemeLattice
  val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)

trait IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeDomain:
  val modularLatticeWrapper = IncrementalSchemeConstantPropagationDomain
