package maf.modular.scheme.modflocal

import maf.core.Position._

//
// general interface for context-sensitivity policy
//

trait SchemeModFLocalSensitivity extends SchemeModFLocal {
  // parameterised by context-sensitivity policy
  type Ctx
  def initialCtx: Ctx
  def newContext(fex: Exp, clo: lattice.Closure, ags: List[(Exp, Val)], ctx: Ctx): Ctx
}

//
// instantiation #1: no context-sensitivity
//

trait SchemeModFLocalNoSensitivity extends SchemeModFLocalSensitivity {
  type Ctx = Unit
  def initialCtx: Unit = ()
  def newContext(fex: Exp, clo: lattice.Closure, ags: List[(Exp, Val)], ctx: Ctx): Ctx = ()
}

//
// instantiation #2: call-site sensitivity (parameterised by k)
//

trait SchemeModFLocalCallSiteSensitivity extends SchemeModFLocalSensitivity {
  // parameterized by some k
  def k: Int
  // context = list of call sites
  type Ctx = List[Position]
  def initialCtx = Nil
  def newContext(fex: Exp, clo: lattice.Closure, ags: List[(Exp, Val)], ctx: Ctx) = 
    (fex.idn.pos :: ctx).take(k)
}
