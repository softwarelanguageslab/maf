package maf.syntax.scheme

import maf.syntax.Source
import maf.syntax.scheme.transformations.{SchemeRenamer, SchemeUndefiner}
import maf.syntax.sexp.*
import maf.util.Reader

object SchemeParser:

    /** Compiles a s-expression into a scheme expression */
    def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

    /** Performs alpha-renaming to ensure that every variable has a unique name */
    def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

    /** Replace defines in a program (a list of expressions) by a big letrec as a single expression */
    def undefine(exps: List[SchemeExp]): SchemeExp = SchemeUndefiner.undefine(exps)

    /** Parse a string representing a Scheme program, which originates from the given source */
    def parse(s: String, source: Source): List[SchemeExp] = SExpParser.parse(s, source).map(compile)

    /** Parse the Scheme program in the given file */
    def parseFile(path: String): List[SchemeExp] = parse(Reader.loadFile(path), Source.File(path))

    /** Parse a program (including adding a prelude and undefining it) */
    def parseProgram(prg: String, source: Source): SchemeExp = undefine(SchemePrelude.addPrelude(parse(prg, source)))
