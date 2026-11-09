package maf.syntax.scheme

import maf.syntax.Source
import maf.syntax.scheme.transformations.{Preluded, SchemePrelude, SchemeRenamer, SchemeUndefiner, UniqueNames}
import maf.syntax.sexp.*
import maf.util.Reader

object SchemeParser:

    /** Compiles a s-expression into a scheme expression */
    def compile(exp: SExp): SchemeExp[Surface] = SchemeCompiler.compile(exp)

    /** Performs alpha-renaming to ensure that every variable has a unique name (which requires the program to be undefined and boxed) */
    def rename[M >: Preluded](exp: SchemeExp[Named] & M): SchemeExp[Named] & UniqueNames & M = SchemeRenamer.rename(exp)

    /** Replace defines in a program (a list of expressions) by a big letrec as a single expression */
    def undefine[F, M >: Preluded](exps: List[SchemeExp[F | Define]] & M): SchemeExp[F | Named] & M = SchemeUndefiner.undefine(exps)

    /** Parse a string representing a Scheme program, which originates from the given source (and may need definitions of the prelude) */
    def parse(s: String, source: Source): List[SchemeExp[Surface]] = SExpParser.parse(s, source).map(compile)

    /** Parse the Scheme program in the given file */
    def parseFile(path: String): List[SchemeExp[Surface]] = parse(Reader.loadFile(path), Source.File(path))

    /** Parse a program (including adding a prelude and undefining it) */
    def parseProgram(prg: String, source: Source): SchemeExp[Mutation | Named] & Preluded = undefine(SchemePrelude.addPrelude(parse(prg, source)))
