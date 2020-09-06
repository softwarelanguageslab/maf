package scalaam.modular.contracts

import scalaam.language.contracts.ScExp

trait ScSymbolicComponents extends ScModSemantics {
  type Sym        = ScExp
  type PC         = ScExp
  type StoreCache = Map[Addr, (Value, Sym)]
  case class PostValue(value: Value, symbolic: Sym)
  def symbolic(value: Value, symbolic: Sym): PostValue = PostValue(value, symbolic)
}
