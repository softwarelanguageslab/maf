package maf.modular.scheme.modf

import maf.core._
import maf.modular.scheme._
import maf.language.scheme._

trait StandardSchemeModFAllocator extends BaseSchemeModFSemantics {
  type AllocationContext = Component
  def allocVar(id: Identifier, cmp: Component) = VarAddr(id,cmp)
  def allocPtr(exp: SchemeExp, cmp: Component) = PtrAddr(exp,cmp)  
}

