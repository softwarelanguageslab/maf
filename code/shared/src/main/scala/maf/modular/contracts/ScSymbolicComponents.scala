package maf.modular.contracts

import maf.core.Identity
import maf.language.contracts.ScExp

trait ScSymbolicComponents extends ScModSemantics {
  type Sym        = ScExp
  type PC         = ScExp
  type StoreCache = Map[Addr, (Value, Sym)]
  case class PostValue(value: Value, symbolic: Sym, idn: Identity = Identity.none)
  def symbolic(value: Value, symbolic: Sym, idn: Identity = Identity.none): PostValue =
    PostValue(value, symbolic, idn)
}
