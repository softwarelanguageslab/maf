package maf.web

import maf.modular.contracts.ScSmtSolver

import maf.modular.contracts.{
  Call,
  ScCallInsensitivity,
  ScConstantPropagationDomain,
  ScGenericAddr,
  ScSmallStepSemantics,
  ScSmtSolver,
  ScVarAddr,
  SimpleScSemantics
}

import maf.language.contracts._

abstract class ScTestAnalysis(prg: ScExp)
    extends SimpleScSemantics(prg)
    with ScCallInsensitivity
    with ScConstantPropagationDomain {}

class ScSMTSolverWeb extends ScSmtSolver {
  def isSat: Boolean = true
}

class ScTestAnalysisWeb(prog: ScExp) extends ScTestAnalysis(prog) {
  override type SMTSolver = ScSMTSolverWeb
  override def newSmtSolver(program: PC): ScSMTSolverWeb =
    new ScSMTSolverWeb()
}
