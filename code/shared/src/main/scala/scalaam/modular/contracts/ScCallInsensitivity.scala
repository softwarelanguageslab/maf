package scalaam.modular.contracts

import scalaam.core.Position
import scalaam.core.Position.Position
import scalaam.language.contracts.{ScIdentifier, ScLattice}

trait ScCallInsensitivity extends ScModSemantics {
  type AllocationContext = Component
  def allocVar(id: ScIdentifier, cmp: Component): ScVarAddr[AllocationContext] = ScVarAddr(id, cmp)
  def allocGeneric(cmp: Component): ScGenericAddr[AllocationContext]           = ScGenericAddr(cmp)

  type ComponentContext = Unit
  def allocCtx(
      clo: ScLattice.Clo[Addr],
      args: List[Value],
      call: Position,
      caller: Component
  ): ComponentContext = ()
}
