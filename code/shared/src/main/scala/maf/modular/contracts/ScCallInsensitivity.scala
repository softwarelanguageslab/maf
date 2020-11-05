package maf.modular.contracts

import maf.core.Identity
import maf.core.Position.Position
import maf.language.contracts.{ScIdentifier, ScLattice}

trait ScCallInsensitivity extends ScModSemantics {
  type AllocationContext    = Component
  type VarAllocationContext = ComponentContext
  def allocVar(id: ScIdentifier, cmp: ComponentContext): ScVarAddr[VarAllocationContext] =
    ScVarAddr(id, cmp)
  def allocGeneric(idn: Identity, cmp: Component): ScGenericAddr[AllocationContext] =
    ScGenericAddr(idn, cmp)

  type ComponentContext = Unit
  def allocCtx(
      clo: ScLattice.Clo[Addr],
      args: List[Value],
      call: Position,
      caller: Component
  ): ComponentContext = ()

  def context(_cmp: Component): ComponentContext = ()
}
