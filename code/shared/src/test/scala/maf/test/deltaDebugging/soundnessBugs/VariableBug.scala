package maf.test.deltaDebugging.soundnessBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait VariableBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  implicit val analysisM: AnalysisM[A]
  import analysisM._

  override protected def evalVariable(nam: String): A[Val] =
    for
      adr <- lookupEnv(nam)
      vlu <- lookupSto(adr)
    yield lattice.join(lattice.number(3), lattice.bool(true))