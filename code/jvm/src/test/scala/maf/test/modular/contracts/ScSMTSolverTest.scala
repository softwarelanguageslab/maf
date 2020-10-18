package maf.test.modular.contracts

import maf.modular.contracts.ScSMTSolverJVM
import maf.test.ScTestsJVM

class ScSMTSolverTest extends ScTestsJVM {
  private def test(expression: String, message: String)(f: ScSMTSolverJVM => Unit): Unit = {
    expression.should(message).in {
      val pc     = compile(expression)
      val solver = new ScSMTSolverJVM(pc, primitivesMap)
      f(solver)
    }
  }

  private def isSat(expression: String): Unit = {
    test(expression, "be satisfiable") { solver =>
      solver.isSat shouldEqual true
    }
  }

  private def isUnsat(expression: String): Unit = {
    test(expression, "be unsatisfiable") { solver =>
      solver.isSat shouldEqual false
    }
  }

  isSat("(> x 0)")
  isSat("(and (int? x) (and (> x 0) (and (< x 5) ())))")
  isUnsat("(and (> x 0) (< x 0))")
  isSat("(string? \"abc\")")
  isUnsat("(string? 2)")
  isSat("(nonzero? x)")
  isUnsat("(nonzero? 0)")
  isSat("(and (any? x) (string? x))")
  isSat("(and (any? x) (int? x))")
}
