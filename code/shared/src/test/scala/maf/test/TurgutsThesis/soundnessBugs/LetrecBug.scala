package maf.test.TurgutsThesis.soundnessBugs

import maf.core.Identifier
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait LetrecBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  implicit val analysisM: AnalysisM[A]
  import analysisM._

  override protected def evalLetrec(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] =
    super.evalLet(bds, bdy)

