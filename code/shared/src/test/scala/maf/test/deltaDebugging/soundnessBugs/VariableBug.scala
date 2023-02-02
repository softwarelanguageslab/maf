package maf.test.deltaDebugging.soundnessBugs

import maf.core.*
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait VariableBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  import analysisM_._

  override protected def evalVariable(vrb: Identifier): A[Val] =
    for
      adr <- lookupEnv(vrb)
      vlu <- lookupSto(adr)
    yield lattice.join(lattice.number(3), lattice.bool(true))
