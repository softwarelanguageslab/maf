package scalaam.test.modular.contracts

import scalaam.modular.contracts.ScSMTSolverJVM
import scalaam.test.ScTestsJVM

class ScSMTSolverTest extends ScTestsJVM {
  private val primitives = Map(
    ">"        -> ">/c",
    "="        -> "=/c",
    "<"        -> "</c",
    "string=?" -> "string=?/c",
    "int?"     -> "int?/c",
    "string?"  -> "string?/c"
  )

  "(> x 0)" should "be satisfiable" in {
    val pc     = compile("(> x 0)")
    val solver = new ScSMTSolverJVM(pc, primitives)
    solver.isSat shouldEqual true
  }

  "Nested ands" should "parse correctly as an SMT formula" in {
    val pc     = compile("(and (int? x) (and (> x 0) (and (< x 5) ())))")
    val solver = new ScSMTSolverJVM(pc, primitives)
    solver.isSat shouldEqual true
  }

  "(and (> x 0) (< x 0))" should "be unsatisfiable" in {
    val pc     = compile("(and (> x 0) (< x 0))")
    val solver = new ScSMTSolverJVM(pc, primitives)
    solver.isSat shouldEqual false
  }

  "(string? \"abc\")" should "be satisfiable" in {
    val pc     = compile("(string? \"abc\")")
    val solver = new ScSMTSolverJVM(pc, primitives)
    solver.isSat shouldEqual true
  }

  "(string? 2)" should "be unsatisfiable" in {
    val pc     = compile("(string? 2)")
    val solver = new ScSMTSolverJVM(pc, primitives)
    solver.isSat shouldEqual false
  }
}
