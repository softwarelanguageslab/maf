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

    /**
     * The prefix of reserved identifiers, which name the definitions that program transformations refer to (such as `#%cons`). Programs can not
     * contain these identifiers, so that they can not define, assign or shadow them. (R5RS does not allow identifiers to start with `#` either.)
     */
    val reservedPrefix: String = "#%"

    /** Parse a string representing a Scheme program, which originates from the given source (and may need definitions of the prelude) */
    def parse(s: String, source: Source): List[SchemeExp[Surface]] =
        val sexps = SExpParser.parse(s, source)
        sexps.foreach(rejectReserved)
        sexps.map(compile)

    /**
     * Parse a string that may contain reserved identifiers, such as a printed program (which may refer to definitions that transformations use, e.g.,
     * for quasiquotes)
     */
    def parseInternal(s: String, source: Source): List[SchemeExp[Surface]] = SExpParser.parse(s, source).map(compile)

    private def rejectReserved(sexp: SExp): Unit = sexp match
        case SExpId(id) if id.name.startsWith(reservedPrefix) =>
            throw new SchemeCompiler.SchemeCompilerException(s"Reserved identifier: ${id.name}", id.pos)
        case SExpPair(car, cdr, _) =>
            rejectReserved(car)
            rejectReserved(cdr)
        case SExpVector(elements, _) => elements.foreach(rejectReserved)
        case _                       => ()

    /** Parse the Scheme program in the given file */
    def parseFile(path: String): List[SchemeExp[Surface]] = parse(Reader.loadFile(path), Source.File(path))

    /** Parse a program (including adding a prelude and undefining it) */
    def parseProgram(prg: String, source: Source): SchemeExp[Mutation | Named] & Preluded = undefine(SchemePrelude.addPrelude(parse(prg, source)))
