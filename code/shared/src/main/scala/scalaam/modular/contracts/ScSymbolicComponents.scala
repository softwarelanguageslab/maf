package scalaam.modular.contracts

import scalaam.core.Identity
import scalaam.language.contracts.ScExp

trait ScSymbolicComponents extends ScModSemantics {
  type Sym        = ScExp
  type PC         = ScExp
  type StoreCache = Map[Addr, (Value, Sym)]
  case class PostValue(value: Value, symbolic: Sym, idn: Identity = Identity.none)
  def symbolic(value: Value, symbolic: Sym, idn: Identity = Identity.none): PostValue =
    PostValue(value, symbolic, idn)
}
