package maf.language.ContractScheme

import maf.language.sexp.SExpParser
import maf.language.scheme._
import maf.core.Position._

object ContractSchemeParser {
  /**
   * Parses a Scheme program with contracts into a scheme body 
   */
  def parse(program: String, tag: PTag = noTag): SchemeExp =
    ContractSchemeUndefiner.undefine(List(SchemeBody(SExpParser.parse(program, tag).map(ContractSchemeCompiler.compile))))
}

