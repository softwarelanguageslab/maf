package maf.language.CScheme

import maf.language.scheme._

object CSchemeLexicalAddresser extends BaseSchemeLexicalAddresser:

    override def translate(exp: SchemeExp, lenv: LexicalEnv): SchemeExp = exp match
        case CSchemeFork(exp, idn) => CSchemeFork(translate(exp, lenv), idn)
        case CSchemeJoin(exp, idn) => CSchemeJoin(translate(exp, lenv), idn)

        case SchemeCodeChange(old, nw, idn) => SchemeCodeChange(translate(old, lenv), translate(nw, lenv), idn)

        case _ => super.translate(exp, lenv)
