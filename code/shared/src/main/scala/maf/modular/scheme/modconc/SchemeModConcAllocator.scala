package maf.modular.scheme.modconc

import maf.core._
import maf.modular.scheme._
import maf.language.scheme._

trait StandardSchemeModConcAllocator extends SchemeModConcSemantics {
  type AllocationContext = InnerModFAnalysis#SchemeModFComponent
  def allocVar(
      id: Identifier,
      modfCmp: InnerModFAnalysis#SchemeModFComponent,
      cmp: Component
    ) = VarAddr(id, modfCmp)
  def allocPtr(
      exp: SchemeExp,
      modfCmp: InnerModFAnalysis#SchemeModFComponent,
      cmp: Component
    ) = PtrAddr(exp, modfCmp)
}
