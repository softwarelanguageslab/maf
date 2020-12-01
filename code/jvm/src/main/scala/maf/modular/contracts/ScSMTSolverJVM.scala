package maf.modular.contracts

import maf.language.contracts.{ScExp, ScFunctionAp, ScIdentifier, ScNil, ScValue}
import maf.language.sexp.{ValueBoolean, ValueInteger, ValueString}

/**
  * Transforms a condition built using basic predicates from the soft contract language
  * into valid assertions for Z3,
  * @param condition the condition which must be checked
  * @param primitives: a map of primitives to names in Z3
  */
class ScSMTSolverJVM(condition: ScExp, primitives: Map[String, String] = Map())
    extends ScSmtSolver {

  val DEBUG_MODE = false

  import com.microsoft.z3._
  import ScSMTSolverJVM._
  import maf.util.MonoidInstances._

  private var variables: List[String] = List()

  object ScAnd {
    def unapply(exp: ScExp): Option[(ScExp, ScExp)] = exp match {
      case ScFunctionAp(ScIdentifier("and", _), List(e1, e2), _, _) => Some((e1, e2))
      case _                                                        => None
    }
  }

  private val prelude: String =
    """
  (declare-datatypes ()
    ((V (VInt  (unwrap-int Int))
        (VBool (unwrap-bool Bool))
        (VProc (unwrap-proc Int))
        (VPai  (car V) (cdr V))
        (VString (unwrap-string String))
        (VPrim (unwrap-prim String)))))

  (declare-fun char/c (V) V)

  (define-fun >/c ((v1 V) (v2 V)) V
     (VBool (> (unwrap-int v1) (unwrap-int v2))))
     
  (define-fun </c ((v1 V) (v2 V)) V
     (VBool (< (unwrap-int v1) (unwrap-int v2))))

  (define-fun =/c ((v1 V) (v2 V)) V
     (VBool (= (unwrap-int v1) (unwrap-int v2))))
     
  (define-fun string=?/c ((v1 V) (v2 V)) V
     (VBool (= (unwrap-string v1) (unwrap-string v2))))
     
  (define-fun int?/c ((v1 V)) V
     (VBool ((_ is VInt) v1)))
     
  (define-fun bool?/c ((v1 V)) V
     (VBool ((_ is VBool) v1)))
     
  (define-fun string?/c ((v1 V)) V
     (VBool ((_ is VString) v1)))
     
  (define-fun proc?/c ((v1 V)) Bool
     (or ((_ is VPrim) v1)
         ((_ is VProc) v1)))
         
   (define-fun nonzero?/c ((v1 V)) V
     (VBool (not (= (unwrap-int v1) 0))))

   ;; everything is true except false
   (define-fun true?/c ((v1 V)) Bool
     (or ((_ is VInt) v1)
         ((_ is VProc) v1)
         ((_ is VPrim) v1)
         ((_ is VString) v1)
         (and ((_ is VBool) v1)
              (unwrap-bool v1))))

    ;; only false is false
    (define-fun false?/c ((v1 V)) Bool
      (not (unwrap-bool v1)))
      
   (define-fun pair?/c ((v1 V)) V
     (VBool ((_ is VPai) v1)))

   (define-fun car/c ((v1 V)) V
     (car v1))

   (define-fun cdr/c ((v1 V)) V
     (cdr v1))

    (define-fun -/c ((v1 V) (v2 V)) V
      (VInt (- (unwrap-int v1) (unwrap-int v2))))
    
    (define-fun +/c ((v1 V) (v2 V)) V
      (VInt (- (unwrap-int v1) (unwrap-int v2))))
      
    (define-fun */c ((v1 V) (v2 V)) V
      (VInt (- (unwrap-int v1) (unwrap-int v2))))
      
    (define-fun //c ((v1 V) (v2 V)) V
      (VInt (- (unwrap-int v1) (unwrap-int v2))))
      
    (define-fun or/c ((v1 V) (v2 V)) V
      (VBool (or (true?/c v1) (true?/c v2)))) 
      
    (define-fun and/c ((v1 V) (v2 V)) V
      (VBool (and (true?/c v1) (true?/c v2))))
      
    (define-fun not/c ((v1 V)) V
      (VBool (not (true?/c v1))))
      
    (define-fun bool?/c ((v1 Bool)) V
     (VBool true))
         
   (define-fun any?/c ((v1 V)) V
     (VBool true))
    """.stripMargin

  def transformExpression(exp: ScExp, operand: Boolean = false): Option[String] = {
    exp match {
      case ScIdentifier(name, _) =>
        primitives.get(name) match {
          case Some(primitiveName) if operand => Some(primitiveName)
          case _ =>
            variables = name :: variables
            Some(name)
        }
      case ScValue(value, _) =>
        value match {
          case ValueString(v)  => Some("(VString \"" + v + "\")")
          case ValueInteger(v) => Some(s"(VInt $v)")
          case ValueBoolean(v) => Some(s"(VBool $v)")
        }
      case ScFunctionAp(operator, operands, _, _) =>
        for {
          transformedOperator <- transformExpression(operator, operand = true)
          transformedOperands <- combineAllNonEmpty(operands.map(e => transformExpression(e)))
        } yield (s"($transformedOperator $transformedOperands)")

      case ScNil(_) => None
    }
  }

  def transform(exp: ScExp): String = exp match {
    case ScAnd(e1, e2) =>
      transform(e1) ++ transform(e2)
    case _: ScFunctionAp =>
      transformExpression(exp) match {
        case Some(s) => s"(assert $s)\n"
        case None    => ""
      }
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
    val userCode = transformed

    if (DEBUG_MODE) {
      println(userCode)
    }
    val smtCode = prelude ++ userCode

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
    val result = check == Status.SATISFIABLE || check == Status.UNKNOWN
    if (DEBUG_MODE) {
      println("SMT SOLVER CHECK", result)
      println("Checked path ", condition)
      println(
        "====================================================================================================="
      )
    }
    result
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
