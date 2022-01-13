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
    final override lazy val lattice = modularLatticeWrapper.modularLattice.incrementalSchemeLattice
    final override lazy val primitives = modularLatticeWrapper.primitives

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
    final val modularLattice = new IncrementalModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)

trait IncrementalSchemeTypeDomain extends IncrementalModularSchemeDomain:
    final val modularLatticeWrapper = IncrementalSchemeTypeDomain
    override def domainName: String = "incremental modular type Scheme domain"

object IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeLatticeWrapper:
    type S = ConstantPropagation.S
    type B = ConstantPropagation.B
    type I = ConstantPropagation.I
    type R = ConstantPropagation.R
    type C = ConstantPropagation.C
    type Sym = ConstantPropagation.Sym
    final val modularLattice = new IncrementalModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.incrementalSchemeLattice)

trait IncrementalSchemeConstantPropagationDomain extends IncrementalModularSchemeDomain:
    final val modularLatticeWrapper = IncrementalSchemeConstantPropagationDomain
    override def domainName: String = "incremental modular constant propagation Scheme domain"
