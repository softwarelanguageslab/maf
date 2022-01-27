package maf.language.ContractScheme

import maf.language.sexp.SExpParser
import maf.language.scheme._
import maf.core.Position._
import maf.language.scheme.primitives.SchemePrelude

/** Main access to the ContractScheme parsing and compilation facilities */
object ContractSchemeParser:
    /**
     * Preprocess the program text so that it accepted by `parse` and `compile`
     *
     * @param program
     *   the program text in S-expression form
     * @return
     *   a transformed program text
     */
    def preprocess(program: String): String =
      if program.startsWith("#lang racket") then program.split('\n').map(_.nn).toList.tail.mkString("\n") else program

    /** Compiles the given program to a ContractScheme AST */
    def compile(program: String, tag: PTag = noTag): SchemeExp =
        val textualPreprocessedProgram = preprocess(program)
        SchemeBody(SExpParser.parse(textualPreprocessedProgram, tag).map(ContractSchemeCompiler.compile))

    /** Parses a Scheme program with contracts into a scheme body, and runs the undefiner on top of it */
    def parse(program: String, tag: PTag = noTag): SchemeExp =
      ContractSchemeUndefiner.undefine(ContractSchemePrelude.addPrelude(List(compile(program, tag))))
