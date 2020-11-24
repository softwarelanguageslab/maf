package maf.modular.incremental.scheme.modf

import maf.language.change.CodeVersion._
import maf.language.scheme.{SchemeCodeChange, SchemeExp}
import maf.modular.AddrDependency
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.scheme.modf.EvalM._
import maf.modular.scheme.modf._
import maf.util.Annotations.nonMonotonicUpdate

trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics {

  @nonMonotonicUpdate
  override def deleteComponent(cmp: Component): Unit = { // This cannot directly go into the intra-component analysis, as the values will then again become joined when the store is committed...
    if (log) logger.log(s"RMCM* $cmp")
    store -= returnAddr(cmp)
    deps -= AddrDependency(returnAddr(cmp))
    super.deleteComponent(cmp)
  }

  trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra with IncrementalIntraAnalysis {
    override protected def eval(exp: SchemeExp): EvalM[Value] = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerComponent(e, component)
        eval(e) // TODO: shouldn't this also be a super call?
      case SchemeCodeChange(_, e, _) if version == New =>
        registerComponent(e, component)
        eval(e) // TODO: shouldn't this also be a super call?
      case _                                     =>
        registerComponent(exp, component)
        super.eval(exp)
    }
  }
}
