package maf.modular.scheme.modflocal

import maf.core.Position._
import maf.modular.scheme._

//
// general interface for context-sensitivity policy
//

trait SchemeModFLocalSensitivity extends SchemeSemantics { this: SchemeDomain =>
  // parameterised by context-sensitivity policy
  type Ctx
  def initialCtx: Ctx
  def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx
}

//
// instantiation #1: no context-sensitivity
//

trait SchemeModFLocalNoSensitivity extends SchemeModFLocalSensitivity { this: SchemeDomain =>
  type Ctx = Unit
  def initialCtx: Unit = ()
  def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx): Ctx = ()
}

//
// instantiation #2: call-site sensitivity (parameterised by k)
//

trait SchemeModFLocalCallSiteSensitivity(k: Int) extends SchemeModFLocalSensitivity { this: SchemeDomain =>
  // context = list of call sites
  type Ctx = List[Position]
  def initialCtx = Nil
  def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx) =
    (fex.idn.pos :: ctx).take(k)
}
