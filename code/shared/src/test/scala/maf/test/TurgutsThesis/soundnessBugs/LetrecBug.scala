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
    val (vrs, rhs) = bds.unzip
    for
      ads <- vrs.mapM(allocVar)
      res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
        for
          _ <- ads.zip(rhs).take(2).mapM_ { case (adr, rhs) =>  //bug is here: take 2
            eval(rhs).flatMap(vlu => extendSto(adr, vlu))
          }
          vlu <- evalSequence(bdy)
        yield vlu
      }
    yield res

