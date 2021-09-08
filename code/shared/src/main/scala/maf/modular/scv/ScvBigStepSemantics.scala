package maf.modular.scv

import maf.language.scheme._
import maf.modular.ModAnalysis
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.modular.scheme.modflocal.SchemeSemantics
import maf.util.benchmarks.Timeout

/** This trait encodes the semantics of the ContractScheme language */
trait ScvBigStepSemantics extends ScvModAnalysis with ScvBaseSemantics { outer =>
  import maf.core.Monad.MonadSyntaxOps
  import scvMonadInstance._

  override def intraAnalysis(component: Component): IntraScvSemantics

  trait IntraScvSemantics extends IntraScvAnalysis with BigStepModFIntraT:
      override def analyzeWithTimeout(timeout: Timeout.T): Unit =
        eval(program).run(State.empty)

      override def eval(exp: SchemeExp): EvalM[Value] = exp match {
        case _ => super.eval(exp)
      }
}
