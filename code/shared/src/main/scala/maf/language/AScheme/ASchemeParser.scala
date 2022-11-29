package maf.language.AScheme

import maf.core.Position._
import maf.language.scheme.SchemeExp
import maf.language.sexp._

object ASchemeParser:
    def parse(s: String, tag: PTag = noTag): List[SchemeExp] =
        SExpParser.parse(s, tag).map(ASchemeCompiler.compile)

    def parseProgram(prg: String, tag: PTag = noTag): SchemeExp =
        ASchemeUndefiner.undefine(ASchemePrelude.addPrelude(ASchemeParser.parse(prg, tag)))
