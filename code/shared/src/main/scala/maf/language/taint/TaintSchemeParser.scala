package maf.language.taint

import maf.core.Position.*
import maf.language.scheme.*
import maf.language.sexp.*
import maf.language.scheme.primitives.SchemePrelude

object TaintSchemeParser:

    def parse(s: String, tag: PTag = noTag): List[SchemeExp] =
        SExpParser.parse(s, tag).map(TaintSchemeCompiler.compile)

    def parseProgram(prg: String, tag: PTag = noTag): SchemeExp =
        SchemeUndefiner.undefine(SchemePrelude.addPrelude(parse(prg, tag)))
