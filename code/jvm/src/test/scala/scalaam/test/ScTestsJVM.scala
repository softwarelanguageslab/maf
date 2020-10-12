package maf.test

import maf.language.contracts.ScExp
import maf.modular.contracts.{ScSMTSolverJVM, ScSmtSolver}

trait ScTestsJVM extends ScTests {
  val primitivesMap = Map(
    ">"        -> ">/c",
    "="        -> "=/c",
    "<"        -> "</c",
    "string=?" -> "string=?/c",
    "int?"     -> "int?/c",
    "string?"  -> "string?/c",
    "nonzero?" -> "nonzero?/c",
    "any?"     -> "any?/c"
  )

  trait ScAnalysisFixtureJVM extends ScAnalysisFixture {
    class ScTestAnalysisJVM(prog: ScExp) extends ScTestAnalysis(prog) {
      override def newSmtSolver(program: PC): ScSMTSolverJVM =
        new ScSMTSolverJVM(program, primitivesMap)
    }
  }
}
