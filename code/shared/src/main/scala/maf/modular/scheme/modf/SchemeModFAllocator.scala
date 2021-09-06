package maf.modular.scheme.modf

import maf.core._
import maf.modular.scheme._
import maf.language.scheme._

// by default, allocation context = context of the function call
trait StandardSchemeModFAllocator extends BaseSchemeModFSemantics:
    type AllocationContext = Option[ComponentContext]
    def allocVar(id: Identifier, cmp: Component) = VarAddr(id, context(cmp))
    def allocPtr(exp: SchemeExp, cmp: Component) = PtrAddr(exp, context(cmp))

// the "old", more precise allocator, where allocation context = the entire component
trait ComponentSchemeModFAllocator extends BaseSchemeModFSemantics:
    type AllocationContext = Component
    def allocVar(id: Identifier, cmp: Component) = VarAddr(id, cmp)
    def allocPtr(exp: SchemeExp, cmp: Component) = PtrAddr(exp, cmp)
