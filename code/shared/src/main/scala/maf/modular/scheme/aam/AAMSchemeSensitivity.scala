package maf.modular.scheme.aam

import maf.modular.scheme.SchemeDomain


//
// context-sensitivity 
//

trait AAMSchemeSensitivity extends AAMScheme:
    this: SchemeDomain =>
    // context is parameterisable 
    type Context
    def t0: Context
    def tnext(app: App, clo: Clo, ctx: Ctx): Context

trait AAMNoSensitivity extends AAMSchemeSensitivity:
    this: SchemeDomain =>
    type Context = Unit
    def t0 = ()
    def tnext(app: App, clo: Clo, ctx: Ctx): Context = ()

trait AAMCallSiteSensitivity(k: Int) extends AAMSchemeSensitivity:
    this: SchemeDomain =>
    type Context = List[App]
    def t0 = Nil
    def tnext(app: App, clo: Clo, ctx: Ctx): Context = (app :: ctx).take(k)
