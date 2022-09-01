package maf.modular.scheme.modactor

import maf.modular.scheme.modf.SchemeModFComponent
import maf.language.scheme.*
import maf.core.*
import maf.modular.scheme.{PtrAddr, VarAddr}
import maf.modular.scheme.SchemeAddr

trait ModActorNoSensitivity extends SchemeModActorSemantics:
    type ComponentContext = Unit
//override def allocCtx(currCmp: Component, idn: Identity): ComponentContext = ()

trait StandardSchemeModActorAllocator extends SchemeModActorSemantics:
    type AllocationContext = (Component, SchemeModFComponent)

    def allocVar(
        id: Identifier,
        modfCmp: SchemeModFComponent,
        cmp: Component
      ) = VarAddr(id, (cmp, modfCmp))
    def allocPtr(
        exp: SchemeExp,
        modfCmp: SchemeModFComponent,
        cmp: Component
      ) = PtrAddr(exp, (cmp, modfCmp))
