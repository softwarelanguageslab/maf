package maf.test

import maf.language.contracts.ScExp
import maf.language.contracts.ScLattice.Blame
import maf.modular.contracts.{ScSMTSolverJVM, ScSmtSolver}

trait ScTestsJVM extends ScTests with ScAnalysisTests {
  class ScTestAnalysisJVM(prog: ScExp) extends ScTestAnalysis(prog) {
    override def newSmtSolver(program: PC): ScSMTSolverJVM =
      new ScSMTSolverJVM(program, primitivesMap)
  }

  def newAnalysis(program: ScExp): ScTestAnalysis =
    new ScTestAnalysisJVM(program)
}
