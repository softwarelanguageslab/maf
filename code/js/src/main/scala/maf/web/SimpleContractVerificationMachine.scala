package maf.web

import maf.language.contracts._
import maf.modular.contracts.{
  ScCallInsensitivity,
  ScConstantPropagationDomain,
  ScSmtSolver,
  SimpleScSemantics
}

abstract class ScTestAnalysis(prg: ScExp)
    extends SimpleScSemantics(prg)
    with ScCallInsensitivity
    with ScConstantPropagationDomain {}

class ScSMTSolverWeb extends ScSmtSolver {
  def isSat: Boolean = true
}

class ScTestAnalysisWeb(prog: ScExp) extends ScTestAnalysis(prog) {
  override val GLOBAL_STORE_ENABLED: Boolean = false
  override type SMTSolver = ScSMTSolverWeb
  override def newSmtSolver(program: PC): ScSMTSolverWeb =
    new ScSMTSolverWeb()
}
