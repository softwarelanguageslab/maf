package maf.language.contracts.primitives

import maf.language.scheme.primitives.SchemePrimitive
import maf.core.Address

trait ScPrimitive[V, A <: Address] extends SchemePrimitive[V, A]
