package maf.test.deltaDebugging.realBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait RealBug1 extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>
  
  override def allocPai(pai: Exp, car: Val, cdr: Val): A[Val] =
    storeVal(pai, lattice.cons(car, car))
