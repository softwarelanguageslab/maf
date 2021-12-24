package maf.aam.scheme

import maf.core.*
import maf.language.scheme.SchemeFuncall

// 1-m-cfa
trait SchemeAAMCallSiteSensitivity extends maf.aam.scheme.BaseSchemeAAMSemantics:
    type Timestamp = List[Identity]
    val initialTime: Timestamp = List()
    def tick(timestamp: Timestamp, e: Expr, sto: Sto, kont: KonA): Timestamp = timestamp
    def alloc(identity: Identity, env: Env, sto: Sto, kont: KonA, ctx: Timestamp): Address =
      VarAddr(identity, "", ctx)

    def allocCtx(fexp: SchemeFuncall, t: Timestamp): Timestamp = List(fexp.idn)
