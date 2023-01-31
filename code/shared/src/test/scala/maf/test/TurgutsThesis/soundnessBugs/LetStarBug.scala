package maf.test.TurgutsThesis.soundnessBugs

import maf.core.Identifier
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait LetStarBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  import analysisM_._
  
  override protected def evalLetStar(bds: List[(Identifier, Exp)], bdy: List[Exp]): A[Val] =
    val (vrs, rhs) = bds.unzip
    for
      vls <- rhs.mapM(arg => eval(arg))
      ads <- vrs.mapM(allocVar)
      res <- withExtendedEnv(vrs.map(_.name).zip(ads)) {
        for
          _ <- extendSto(ads.zip(vls))
          vlu <- evalSequence(bdy)
        yield vlu
      }
    yield lattice.void //bug on this line
