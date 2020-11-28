package maf.modular.contracts

import maf.core.Identity
import maf.language.contracts.{ScExp, ScProgram}

trait ScAnalysisWithPrelude extends ScModSemantics {
  import maf.language.contracts.ScPrelude._
  override def program: ScExp = {
    val prg = super.program
    prg match {
      case ScProgram(expressions, idn) => ScProgram(expressions ++ prelude, idn)
      case e                           => ScProgram(List(e) ++ prelude, Identity.none)
    }
  }
}
