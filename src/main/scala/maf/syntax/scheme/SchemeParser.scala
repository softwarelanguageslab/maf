package maf.syntax
package scheme

import maf.syntax.sexp.*

object SchemeParser:

  /** Compiles a s-expression into a scheme expression */
  def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

  /** Performs alpha-renaming to ensure that every variable has a unique name */
  def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

  /** Replace defines in a program (a list of expressions) by a big letrec as a
    * single expression
    */
  def undefine(exps: List[SchemeExp]): SchemeExp =
    SchemeUndefiner.undefine(exps)

  /** Parse a string representing a Scheme program */
  def parse(s: String, tag: Option[PTag] = None): List[SchemeExp] =
    SExpParser.parse(s, tag).map(compile)

  /** Parses a program without running "undefine" afterwards */
  def parseProgramText(prg: String): List[SchemeExp] =
    SchemePrelude.addPrelude(parse(prg))

  /** Parse a program (including adding a prelude and undefining it) */
  def parseProgram(prg: String): SchemeExp = undefine(
    SchemePrelude.addPrelude(parse(prg))
  )
