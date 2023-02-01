package maf.test.deltaDebugging.soundnessBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait CallBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  implicit val analysisM: AnalysisM[A]
  import analysisM._

  override protected def evalCall(app: App): A[Val] =
    for
      fun <- eval(app.f)
      ags <- app.args.take(2).mapM(arg => eval(arg)) //bug is here: arguments only up to 3 supported
      res <- applyFun(app, fun, ags)
    yield res
    
