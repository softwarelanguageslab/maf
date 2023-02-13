package maf.test.deltaDebugging.realBugs

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.{SchemeModFLocalSensitivity, SchemeSemantics}

trait RealBug2 extends SchemeSemantics:
  this: SchemeDomain with SchemeModFLocalSensitivity =>

  override protected def applyFun(app: App, fun: Val, ags: List[Val]): A[Val] =
    applyClosures(app, fun, ags) //type-preserving operation is omitted, that deals with primitives
