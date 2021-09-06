package maf.modular.scheme.modconc

import maf.language.scheme._
import maf.modular.scheme.modf._

trait SchemeModConcNoSensitivity extends SchemeModConcSemantics:
    type ComponentContext = NoContext.type
    def allocCtx(
        exp: SchemeExp,
        env: Env,
        modFCmp: SchemeModFComponent,
        caller: Component
      ) = NoContext

trait SchemeModConcStandardSensitivity extends SchemeModConcSemantics:
    type ComponentContext = SchemeModFComponent
    def allocCtx(
        exp: SchemeExp,
        env: Env,
        modFCmp: SchemeModFComponent,
        caller: Component
      ) = modFCmp
