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
    override def configString(): String = super.configString() + "\n  without context sensitivity"

trait SchemeModConcStandardSensitivity extends SchemeModConcSemantics:
    type ComponentContext = SchemeModFComponent
    def allocCtx(
        exp: SchemeExp,
        env: Env,
        modFCmp: SchemeModFComponent,
        caller: Component
      ) = modFCmp
    override def configString(): String = super.configString() + "\n  with standard ModConc context sensitivity"
