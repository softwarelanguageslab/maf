package scalaam.modular.contracts

import scalaam.language.contracts.{ScExp, ScFunctionAp, ScIdentifier, ScNil, ScValue}

/**
  * Transforms a condition built using basic predicates from the soft contract language
  * into valid assertions for Z3,
  * @param condition the condition which must be checked
  * @param primitives: a map of primitives to names in Z3
  */
class ScSMTSolverJVM(condition: ScExp, primitives: Map[String, String] = Map())
    extends ScSmtSolver {

  import com.microsoft.z3._
  import ScSMTSolverJVM._

  private var variables: List[String] = List()

  object ScAnd {
    def unapply(exp: ScExp): Option[(ScExp, ScExp)] = exp match {
      case ScFunctionAp(ScIdentifier("and", _), List(e1, e2), _) => Some((e1, e2))
      case _                                                     => None
    }
  }

  private val prelude: String = ""

  def transformExpression(exp: ScExp): String = exp match {
    case ScIdentifier(name, _) =>
      primitives.get(name) match {
        case Some(primitiveName) => primitiveName
        case None => {
          variables = name :: variables
          name
        }
      }
    case ScValue(value, _) => value.toString
    case ScFunctionAp(operator, operands, _) =>
      s"(${transformExpression(operator)} ${operands.map(transformExpression).mkString(" ")}"
  }

  def transform(exp: ScExp): String = exp match {
    case ScAnd(e1, e2) =>
      transform(e1) ++ transform(e2)
    case _: ScFunctionAp =>
      s"(assert ${transformExpression(exp)})"
    case _ => ""
  }

  /**
    * Transforms the condition into valid Z3 assertions
    * @return valid Z3 assertions
    */
  private def transformed: String = {
    val assertions = transform(condition)
    val constants  = variables.map(v => s"(declare-const ${v} V)").mkString(" ")
    constants ++ assertions
  }

  /**
    * Checks if the current formula is satisfiable
    * @return true if the formale is satisfiable otherwise false
    */
  def isSat: Boolean = {
    return true

    // transform the code
    val smtCode = prelude ++ transformed

    // create a new context and solver
    val context = new Context()
    val solver  = context.mkSolver()

    // transform the textual representation of the assertions to the internal format of Z3
    val e: Array[BoolExpr] = context.parseSMTLIB2String(smtCode, null, null, null, null)

    // check whether their exist a model which satisfies all the constraints
    solver.assert_(e)
    val check = solver.check()

    // the PC is satisfiable when the SMT solver either indicates that the model is satisfiable or
    // it does not know if it is satisfiable.
    //
    // the unknown case is important as we are making an overapproximation, it would be unsound if we would ignore
    // paths that are not known with the SMT solver to be satisfiable
    check == Status.SATISFIABLE || check == Status.UNKNOWN
  }
}

object ScSMTSolverJVM {
  import com.microsoft.z3._
  implicit class Z3Solver(solver: Solver) {
    def assert_(expressions: Array[BoolExpr]): Unit = {
      for (expression <- expressions) {
        solver.add(expression)
      }
    }
  }
}
