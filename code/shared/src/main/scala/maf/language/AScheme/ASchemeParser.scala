package maf.language.AScheme

import maf.core.Position.*
import maf.core.Identity
import maf.language.scheme.*
import maf.language.sexp.*

object ASchemeParser:
    def parse(s: String, tag: PTag = noTag): List[SchemeExp] =
        SExpParser.parse(s, tag).map(ASchemeCompiler.compile)

    def parseProgram(prg: String, tag: PTag = noTag): SchemeExp =
        ASchemeUndefiner.undefine(ASchemePrelude.addPrelude(ASchemeParser.parse(prg, tag)))

    /** Same as parseProgram but without running the undefiner */
    def parseProgramDefines(prg: String, tag: PTag = noTag): SchemeExp =
        SchemeBegin(ASchemePrelude.addPrelude(ASchemeParser.parse(prg, tag)), Identity.none)
