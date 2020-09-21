package maf.modular.scheme.modconc

import maf.core._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.language.scheme._

trait StandardSchemeModConcAllocator extends SchemeModConcSemantics {
  type AllocationContext = SchemeModFComponent
  def allocVar(id: Identifier, modfCmp: SchemeModFComponent, cmp: Component) = VarAddr(id,modfCmp)
  def allocPtr(exp: SchemeExp, modfCmp: SchemeModFComponent, cmp: Component) = PtrAddr(exp,modfCmp)  
}
