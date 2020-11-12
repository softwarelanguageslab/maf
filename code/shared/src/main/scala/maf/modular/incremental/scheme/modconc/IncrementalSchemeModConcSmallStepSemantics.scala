package maf.modular.incremental.scheme.modconc

import maf.language.change.CodeVersion._
import maf.language.scheme.SchemeCodeChange
import maf.modular.AddrDependency
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.scheme.ssmodconc._
import maf.util.Annotations.nonMonotonicUpdate

trait IncrementalSchemeModConcSmallStepSemantics extends SmallStepModConcSemantics with IncrementalSchemeSemantics {

  @nonMonotonicUpdate
  override def deleteComponent(cmp: Component): Unit = { // This cannot directly go into the intra-component analysis, as the values will then again become joined when the store is committed...
    // Deletes the return value from the global store if required (sets it to bottom), as well as the corresponding dependencies.
    logger.log(s"deleting* $cmp")
    store -= returnAddr(cmp)
    deps -= AddrDependency(returnAddr(cmp))
    super.deleteComponent(cmp)
  }

  trait IncrementalSmallStepIntra extends SmallStepIntra with IncrementalIntraAnalysis {
    override protected def evaluate(exp: Exp, env: Env, stack: Stack): Set[State] = exp match {
      case SchemeCodeChange(e, _, _) if version == Old =>
        registerComponent(e, component)
        Set(Eval(e, env, stack)) // TODO: shoudn't this also be a super call?
      case SchemeCodeChange(_, e, _) if version == New =>
        registerComponent(e, component)
        Set(Eval(e, env, stack)) // TODO: shoudn't this also be a super call?
      case _                                           =>
        registerComponent(exp, component)
        super.evaluate(exp, env, stack)
    }
  }
}
