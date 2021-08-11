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
  def newContext(fex: Exp, clo: Clo, ags: List[(Exp, Val)], ctx: Ctx): Ctx
}

//
// instantiation #1: no context-sensitivity
//

trait SchemeModFLocalNoSensitivity extends SchemeModFLocalSensitivity { this: SchemeDomain =>
  type Ctx = Unit
  def initialCtx: Unit = ()
  def newContext(fex: Exp, clo: Clo, ags: List[(Exp, Val)], ctx: Ctx): Ctx = ()
}

//
// instantiation #2: call-site sensitivity (parameterised by k)
//

trait SchemeModFLocalCallSiteSensitivity extends SchemeModFLocalSensitivity { this: SchemeDomain =>
  // parameterized by some k
  def k: Int
  // context = list of call sites
  type Ctx = List[Position]
  def initialCtx = Nil
  def newContext(fex: Exp, clo: lattice.Closure, ags: List[(Exp, Val)], ctx: Ctx) = 
    (fex.idn.pos :: ctx).take(k)
}
