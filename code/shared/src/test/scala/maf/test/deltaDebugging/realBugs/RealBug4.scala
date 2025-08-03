package maf.test.deltaDebugging.realBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait RealBug4 extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  import analysisM_._

  override protected def evalSequence(eps: Iterable[Exp]): A[Val] =
    eps match
      case Nil => unit(lattice.void)
      case last :: Nil => eval(last)
      case next :: rest => nontail(???) {
        eval(next)
      } >>> evalSequence(rest.tail) //off-by-one error
