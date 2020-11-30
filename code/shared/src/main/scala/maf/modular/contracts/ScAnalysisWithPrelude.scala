package maf.modular.contracts

import maf.core.Identity
import maf.language.contracts.{ScExp, ScProgram}

trait ScAnalysisWithPrelude extends ScModSemantics {
  import maf.language.contracts.ScPrelude._
  override def program: ScExp = {
    val prg = super.program
    prg match {
      case ScProgram(expressions, idn) => ScProgram(prelude ++ expressions, idn)
      case e                           => ScProgram(prelude ++ List(e), Identity.none)
    }
  }
}
