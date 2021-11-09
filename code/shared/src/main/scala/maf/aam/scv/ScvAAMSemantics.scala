package maf.aam.scv

import maf.aam.scheme.*
import maf.core.Address
import maf.modular.scv.*
import maf.language.scheme.*
import maf.language.ContractScheme.*

trait ScvAAMSemantics extends SchemeAAMSemantics:
    /** We add SCV specific information to the each AAM state */
    type Ext = ScvState
    type StoreCache = Unit
    type PC = PathStore

    case class ScvState(m: StoreCache, phi: PC)

    override protected def cond(
        value: Val,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] = super.cond(value, csq, alt, env, sto, kont, t, ext)

    override def eval(
        exp: SchemeExp,
        env: Env,
        sto: Sto,
        kont: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] = exp match
        case _ => super.eval(exp, env, sto, kont, t, ext)

    override def continue(vlu: Val, sto: Sto, kon: Address, t: Timestamp, ext: Ext): Set[State] =
      super.continue(vlu, sto, kon, t, ext)
