package maf.test.deltaDebugging.soundnessBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait IfBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  override protected def evalIf(prd: Exp, csq: Exp, alt: Exp): A[Val] =
    eval(prd)
