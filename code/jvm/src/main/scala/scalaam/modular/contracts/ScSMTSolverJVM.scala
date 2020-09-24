package scalaam.modular.contracts

import scalaam.language.contracts.{ScExp, ScFunctionAp, ScIdentifier, ScNil, ScValue}
import scalaam.language.sexp.{ValueBoolean, ValueInteger, ValueString}

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

  private val prelude: String =
    """
  (declare-datatypes ()
    ((V (VInt  (unwrap-int Int))
        (VBool (unwrap-bool Bool))
        (VProc (unwrap-proc Int))
        (VString (unwrap-string String))
        (VPrim (unwrap-prim String)))))

  (define-fun >/c ((v1 V) (v2 V)) Bool
     (> (unwrap-int v1) (unwrap-int v2)))
     
  (define-fun </c ((v1 V) (v2 V)) Bool
     (< (unwrap-int v1) (unwrap-int v2)))

  (define-fun =/c ((v1 V) (v2 V)) Bool
     (= (unwrap-int v1) (unwrap-int v2)))
     
  (define-fun string=?/c ((v1 V) (v2 V)) Bool
     (= (unwrap-string v1) (unwrap-string v2)))
     
  (define-fun int?/c ((v1 V)) Bool
     ((_ is VInt) v1))
     
  (define-fun bool?/c ((v1 V)) Bool
     ((_ is VBool) v1))
     
  (define-fun string?/c ((v1 V)) Bool
     ((_ is VString) v1))
     
  (define-fun proc?/c ((v1 V)) Bool
     (or ((_ is VPrim) v1)
         ((_ is VProc) v1)))
    """.stripMargin

  def transformExpression(exp: ScExp): String = exp match {
    case ScIdentifier(name, _) =>
      primitives.get(name) match {
        case Some(primitiveName) => primitiveName
        case None => {
          variables = name :: variables
          name
        }
      }
    case ScValue(value, _) =>
      value match {
        case ValueString(v)  => "(VString \"" + v + "\")"
        case ValueInteger(v) => s"(VInt $v)"
        case ValueBoolean(v) => s"(VBool $v)"
      }

    case ScFunctionAp(operator, operands, _) =>
      s"(${transformExpression(operator)} ${operands.map(transformExpression).mkString(" ")})"
  }

  def transform(exp: ScExp): String = exp match {
    case ScAnd(e1, e2) =>
      transform(e1) ++ transform(e2)
    case _: ScFunctionAp =>
      s"(assert ${transformExpression(exp)})\n"
    case _ => ""
  }

  /**
    * Transforms the condition into valid Z3 assertions
    * @return valid Z3 assertions
    */
  private def transformed: String = {
    val assertions = transform(condition)
    val constants  = variables.toSet.map((v: String) => s"(declare-const ${v} V)").mkString("\n")
    constants ++ "\n" ++ assertions
  }

  /**
    * Checks if the current formula is satisfiable
    * @return true if the formale is satisfiable otherwise false
    */
  def isSat: Boolean = {
    // transform the code
    val smtCode = prelude ++ transformed
    println(smtCode)

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
