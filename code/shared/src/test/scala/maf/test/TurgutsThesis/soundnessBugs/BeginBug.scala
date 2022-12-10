package maf.test.TurgutsThesis.soundnessBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait BeginBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  override protected def evalSequence(eps: Iterable[Exp]): A[Val] =
    eval(eps.head)
