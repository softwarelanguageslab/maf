package maf.language.AScheme

import maf.language.scheme.primitives.BaseSchemePrelude

object ASchemePrelude extends BaseSchemePrelude:
    override def primDefs = super.primDefs ++ List(
      "print-statistics" -> "(define (print-statistics) '())",
      "#lang" -> "(define #lang '())",
      "racket" -> "(define racket '())",
      // ignore all requires for now:
      "require" -> "(define require (lambda ags '()))",
      // ignore the actor module
      "acontracts/actors" -> "(define acontracts/actors '())"
    ).toMap
