package maf.aam.scheme.sensitivity

import maf.aam.scheme.*

import maf.core.*
import maf.language.scheme.SchemeFuncall

trait SchemeAAMContextInsensitivity extends BaseSchemeAAMSemantics:
    type Timestamp = Unit
    val initialTime: Timestamp = ()
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: KonA): Timestamp = ()
    def alloc(identity: Identity, env: Env, sto: Sto, kont: KonA, ctx: Timestamp): Address =
      VarAddr(identity, "", ctx)
    def allocCtx(fexp: SchemeFuncall, t: Timestamp): Timestamp = t
