package maf.modular.adaptive.scheme.adaptiveArgumentSensitivity

import maf.modular.scheme.modf._
import maf.modular.scheme.modf.SchemeModFComponent._

trait AdaptiveArgumentSensitivityPolicy1 extends AdaptiveArgumentSensitivity {
  // parameterized by a simple limit
  // every closure can only have at most "limit" components
  val limit: Int
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    super.onNewComponent(cmp, call)
    // if there are too many components => do something about it!
    val cmps = cmpsPerFn(call.clo._1)
    if (limit < cmps.size) {
      joinComponents(cmps)
    }
  }
}
