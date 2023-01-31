package maf.modular.scheme.aam

import maf.core.*
import maf.modular.scheme.*

trait AAMNameBasedAllocator extends AAMScheme:
    this: SchemeDomain with AAMSchemeSensitivity => 
    override def alloc(v: Var, ctx: Ctx) = VAdr(Identifier(v.name, Identity.none), ctx)
