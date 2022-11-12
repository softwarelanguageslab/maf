package maf.test.TurgutsThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec

class ShallowDropIdentifierTest extends AnyFlatSpec {
  "GTR" should "be able to shallow delete identifiers" in {
    def testLet(programText: String, expected: String = "(let () 'done)"): Unit =
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
      val id = letExp.bindings.head._1
      val shallowDeleted = letExp.shallowDropIdentifier(id)
      shallowDeleted match
        case Some(exp) =>
          //println(exp)
          assert(exp.toString == expected)
        case _ =>

    def testLambda(programText: String, expected: String): Unit =
      val schemeLambdaExp: SchemeLambda = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLambda]
      val arg = schemeLambdaExp.args.head
      val shallowDeleted = schemeLambdaExp.shallowDropIdentifier(arg)
      shallowDeleted match
        case Some(exp) =>
          println(exp)
          //assert(exp.toString == expected)
        case None =>
          assert(expected == "None")

    //code 1: identifier in appl
    val programText1: String =
    """(let
      |    ((x 5))
      |  (+ 10 x)
      |  (x 5 9)
      |  (x x x)
      |  'done)""".stripMargin

    testLet(programText1)

    //code 2: identifier in if
    val programText2: String =
      """(let
        |    ((x 5))
        |  (define y 10)
        |  (if (= x 0) y y)
        |  (if (= y 0) (+ x 10) y)
        |  (if (= y 0) y (+ x 10)))""".stripMargin

    testLet(programText2, "(let () (define y 10))")

    //code 3: identifier in set!
    val programText3: String =
      """(let
        |    ((x 5)
        |     (y 9))
        |  (set! y (+ x 10))
        |  (set! x (+ y y))
        |  'done)""".stripMargin

    testLet(programText3, "(let ((y 9)) 'done)")

    //code 4: identifier in define
    val programText4: String =
      """(let
        |    ((x 5))
        |  (define y (+ 10 (+ x 10)))
        |  'done)""".stripMargin

    testLet(programText4)

    //code 5: identifier in let/letStar/letrec
    val programText5: String =
    """(let
      |    ((x 5))
      |  (let ((y x)) 10)
      |  (let* ((z 10)) x)
      |  'done)""".stripMargin

    testLet(programText5)

    //code 6: identifier in lambda
    val programText6: String =
      """(let
        |    ((x 5))
        |  (lambda (y z) (+ 100 (+ 100 x)))
        |  'done)""".stripMargin

    testLet(programText6)

    //code 7: identifier should not be deleted everywhere

    val programText7: String =
      """(let ((x 5)
        |      (a 10)
        |      (b 100))
        |  (lambda (y z) (+ 100 (+ 100 x)))
        |  (+ a b (* a 10))
        |  (if a a a)
        |  (if a (+ b b) (* b b))
        |  (set! a b))""".stripMargin


    testLet(programText7, "(let ((a 10) (b 100)) (+ a b (* a 10)) (if a a a) (if a (+ b b) (* b b)) (set! a b))")

    //code 8: Let with more than 1 binding
    val programText8: String =
      """(let
        |    ((x 5)
        |     (y (+ 10 x)))
        |  (* y x)
        |  (begin 10 (+ y 10))
        |  'done)
        |""".stripMargin

    testLet(programText8)

    //code 9: Nested let, with nested binding that should be recursively deleted
    val programText9: String =
      """(let ((x 5))
        |  (let ((y 10))
        |    (* y x)
        |    (begin 10 (+ y 10))
        |    'done)
        |  )""".stripMargin

    testLet(programText9)

    //code 10: a lambda
    val programText10: String =
      """(lambda (a b c)
        |  (+ b c (* a a))
        |  (let ((z b))
        |    c)
        |  )""".stripMargin

    testLambda(programText10, "(lambda (b c) (let ((z b)) c))")

    //code 10: a lambda
    val programText11: String =
      """(lambda (a b c)
        |  (+ b c (* a a))
        |  )""".stripMargin

    testLambda(programText11, "None")
  }
}
