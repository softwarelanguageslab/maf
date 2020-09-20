package scalaam.test

import scalaam.language.contracts.ScExp
import scalaam.modular.contracts.{ScSMTSolverJVM, ScSmtSolver}

trait ScTestsJVM extends ScTests {
  trait ScAnalysisFixtureJVM extends ScAnalysisFixture {
    class ScTestAnalysisJVM(prog: ScExp) extends ScTestAnalysis(prog) {
      override def newSmtSolver(program: PC): ScSMTSolverJVM = new ScSMTSolverJVM(program)
    }
  }
}
