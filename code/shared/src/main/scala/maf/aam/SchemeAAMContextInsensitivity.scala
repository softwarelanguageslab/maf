package maf.aam

import maf.core.*

trait SchemeAAMContextInsensitivity extends maf.aam.SchemeAAMSemantics:
    type Timestamp = Unit
    val initialTime: Timestamp = ()
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: Address): Timestamp = ()
    def alloc(identity: Identity, env: Env, sto: Sto, kont: Address, ctx: Timestamp): Address =
      VarAddr(identity, ctx)
