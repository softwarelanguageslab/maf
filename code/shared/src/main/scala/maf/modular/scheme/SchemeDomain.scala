package maf.modular.scheme

import maf.core._
import maf.modular._
import maf.lattice._
import maf.language.scheme._
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeLattice}
import maf.language.scheme.primitives._
import maf.lattice.instances.{BoundedLattice, Concrete, ConstantPropagation, Type}

/** The abstract domain used for Scheme analyses. */
trait SchemeDomain extends AbstractDomain[SchemeExp] {

  /** Type of primitive functions. */
  type Prim = SchemePrimitive[Value, Address]

  /** Contains the implementation of the primitives for the given abstract domain. */
  val primitives: SchemePrimitives[Value, Address]

  /** Implementation of abstract values. */
  implicit val lattice: SchemeLattice[Value, Address]
}

trait ModularSchemeDomain extends SchemeDomain {
  // gets a domain from a modular lattice wrapper
  val modularLatticeWrapper: ModularSchemeLatticeWrapper
  // extracts value and lattice definition from this wrapper
  type Value = modularLatticeWrapper.modularLattice.L
  lazy val lattice = modularLatticeWrapper.modularLattice.schemeLattice
  lazy val primitives = modularLatticeWrapper.primitives
}

trait ModularSchemeLatticeWrapper {
  // consists of several types
  type S
  type B
  type I
  type R
  type C
  type Sym
  // holds a modular lattice
  val modularLattice: ModularSchemeLattice[Address, S, B, I, R, C, Sym]
  val primitives: SchemePrimitives[modularLattice.L, Address]
}

//
// TYPE DOMAIN
//

/* A type lattice for ModF */
object SchemeTypeDomain extends ModularSchemeLatticeWrapper {
  // use type domains everywhere, except for booleans
  type S = Type.S
  type B = ConstantPropagation.B
  type I = Type.I
  type R = Type.R
  type C = Type.C
  type Sym = Type.Sym
  // make the scheme lattice
  lazy val modularLattice = new ModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)
}

trait SchemeTypeDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemeTypeDomain
}

//
// CONSTANT PROPAGATION DOMAIN
//

object SchemeConstantPropagationDomain extends ModularSchemeLatticeWrapper {
  // use constant propagation domains everywhere, except for booleans
  type S = ConstantPropagation.S
  type B = ConstantPropagation.B
  type I = ConstantPropagation.I
  type R = ConstantPropagation.R
  type C = ConstantPropagation.C
  type Sym = ConstantPropagation.Sym
  // make the scheme lattice
  lazy val modularLattice = new ModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)
}

trait SchemeConstantPropagationDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemeConstantPropagationDomain
}

//
// POWERSET DOMAIN
//

/* A powerset lattice for ModF */
object SchemePowersetDomain extends ModularSchemeLatticeWrapper {
  // use powerset domains everywhere
  type S = Concrete.S
  type B = Concrete.B
  type I = Concrete.I
  type R = Concrete.R
  type C = Concrete.C
  type Sym = Concrete.Sym
  // make the scheme lattice
  lazy val modularLattice = new ModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)
}

trait SchemePowersetDomain extends ModularSchemeDomain {
  lazy val modularLatticeWrapper = SchemePowersetDomain
}

//
// BOUNDED SET DOMAIN
//
class SchemeBoundedDomainWrapper(val bound: Int) extends ModularSchemeLatticeWrapper {
  object Bounded extends BoundedLattice(bound)
  type S = Bounded.S
  type B = Bounded.B
  type I = Bounded.I
  type R = Bounded.R
  type C = Bounded.C
  type Sym = Bounded.Sym
  lazy val modularLattice = new ModularSchemeLattice
  lazy val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)
}

trait SchemeBoundedDomain extends ModularSchemeDomain {
  val bound: Int
  lazy val modularLatticeWrapper = new SchemeBoundedDomainWrapper(bound)
}
