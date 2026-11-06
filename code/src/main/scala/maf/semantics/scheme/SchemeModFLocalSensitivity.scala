package maf.semantics.scheme

import maf.domain.scheme.*
import maf.syntax.Expression

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

/** A call site, i.e., a call expression of the program (printed as its position, or as its label if it has none) */
case class CallSite(call: Expression):
    override def toString: String = call.pos.fold(s"#${call.label}")(_.toString)

trait SchemeModFLocalCallSiteSensitivity(k: Int) extends SchemeModFLocalSensitivity { this: SchemeDomain =>
    // context = list of call sites
    type Ctx = List[CallSite]
    def initialCtx = Nil
    def newContext(fex: Exp, lam: Lam, ags: List[Val], ctx: Ctx) =
        (CallSite(fex) :: ctx).take(k)
}
