package maf.test.deltaDebugging.soundnessBugs

import maf.language.scheme.SchemeValue
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}
import maf.language.sexp


trait LiteralValueBug extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  import analysisM_._

  override protected def evalLiteralValue(exp: SchemeValue): A[Val] =
    exp.value match
      case sexp.Value.Integer(n) => unit(lattice.number(n))
      case sexp.Value.Real(r) => unit(lattice.real(r))
      case sexp.Value.Boolean(b) => unit(lattice.bool(b))
      case _ => unit(lattice.bool(true)) //bug is here: other literals are treated like true
