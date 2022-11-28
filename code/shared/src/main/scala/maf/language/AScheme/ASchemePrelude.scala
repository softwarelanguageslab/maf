package maf.language.AScheme

import maf.language.scheme.primitives.BaseSchemePrelude

object ASchemePrelude extends BaseSchemePrelude:
    override def primDefs = super.primDefs ++ List(
      "print-statistics" -> "(define (print-statistics) '())"
    ).toMap
