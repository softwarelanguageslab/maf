package maf.modular.incremental.scheme.modf

import maf.language.change.CodeVersion._
import maf.language.scheme._
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.scheme.modf.EvalM._
import maf.modular.scheme.modf._

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {

  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra with IncrementalIntraAnalysis {
    override protected def eval(exp: SchemeExp): EvalM[Value] = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerComponent(e, component)
        eval(e) // This could also be a super call if we assume no nesting of change expressions (which could be expected).
      case SchemeCodeChange(_, e, _) if version == New =>
        registerComponent(e, component)
        eval(e) // Same than above.
      case _ =>
        registerComponent(exp, component)
        super.eval(exp)
    }
  }
}
