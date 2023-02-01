package maf.test.deltaDebugging.soundnessBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait LambdaBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  implicit val analysisM: AnalysisM[A]
  import analysisM._

  override protected def evalLambda(lam: Lam): A[Val] =
    for env <- getEnv yield lattice.symbolTop //bug is here, were yielding non-lambda values
