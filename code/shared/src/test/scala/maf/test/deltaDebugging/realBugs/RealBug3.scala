package maf.test.deltaDebugging.realBugs

import maf.core.Expression
import maf.core.worklist.{LIFOWorkList, WorkList}
import maf.language.scheme.SchemeValue
import maf.language.sexp
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}
import maf.modular.scheme.*
import maf.modular.worklist.SequentialWorklistAlgorithm
import maf.util.benchmarks.Timeout

trait RealBug3 extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  import maf.core.Monad._
  import maf.core.MonadJoin._

  import analysisM_._

  override protected def evalLiteralValue(exp: SchemeValue): A[Val] =
    exp.value match
      case sexp.Value.String(s) => storeVal(exp, lattice.string(s))
      case sexp.Value.Integer(n) => unit(lattice.real(n.toDouble)) //buggy
      case sexp.Value.Real(r) => unit(lattice.real(r))
      case sexp.Value.Boolean(b) => unit(lattice.bool(b))
      case sexp.Value.Character(c) => unit(lattice.char(c))
      case sexp.Value.Symbol(s) => unit(lattice.symbol(s))
      case sexp.Value.Nil => unit(lattice.nil)
