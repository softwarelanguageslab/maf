package maf.language.CScheme

import maf.core.Position._
import maf.language.scheme._
import maf.language.sexp._
import maf.language.scheme.primitives.SchemePrelude

object CSchemeParser:

    /** Compiles a s-expression into a CScheme expression. */
    def compile(exp: SExp): SchemeExp = CSchemeCompiler.compile(exp)

    /** Replace defines in a program (a list of expressions) by a big letrec as a single expression. */
    def undefine(exps: List[SchemeExp]): SchemeExp = CSchemeUndefiner.undefine(exps)

    /** Parse a string representing a CScheme program. */
    def parse(s: String, tag: PTag = noTag): List[SchemeExp] = SExpParser.parse(s, tag).map(compile)

    /** Parse a program, add its prelude and undefine it */
    def parseProgram(prg: String): SchemeExp = undefine(SchemePrelude.addPrelude(CSchemeParser.parse(prg)))

    /** Extension to the parser to allow loading definitions from different files. */
    def parseL(
        s: String,
        file: String,
        tag: PTag = noTag
      ): SchemeExp = SchemeBody(SExpParser.parse(s, tag).flatMap(e => SchemeLoader.load(file, e)).map(compile))
