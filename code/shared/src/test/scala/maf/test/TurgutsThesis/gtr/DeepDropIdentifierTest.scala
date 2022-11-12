package maf.test.TurgutsThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLet, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec

class DeepDropIdentifierTest extends AnyFlatSpec {
  "GTR" should "be able to deep drop identifiers" in {
    def testLet(programText: String, expected: String = "(let () )"): Unit =
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
      val id = letExp.bindings.head._1
      val deepDropped = letExp.deepDropIdentifier(id)

      deepDropped match
        case Some(letExp) =>
          //println(letExp)
          assert(letExp.toString == expected)
        case _ =>

    def testLambda(programText: String, expected: String): Unit =
      val schemeLambdaExp: SchemeLambda = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLambda]
      val arg = schemeLambdaExp.args.head
      val shallowDeleted = schemeLambdaExp.deepDropIdentifier(arg)
      shallowDeleted match
        case Some(exp) =>
          //println(exp)
          assert(exp.toString == expected)
        case None =>
          //assert(expected == "None")


    //code 1: identifier in appl
    val programText1: String =
      """(let
        |    ((x 5))
        |  (+ 10 x)
        |  (x 5 9)
        |  (x x x))""".stripMargin

    testLet(programText1, "(let () (+ 10) (5 9))")

    //code 2: identifier in if
    val programText2: String =
      """(let
        |    ((x 5))
        |  (define y 10)
        |  (if (= x 0) y y)
        |  (if (= y 0) (+ x 10) y)
        |  (if (= y 0) y (+ x 10)))""".stripMargin

    testLet(programText2, "(let () (define y 10) (if (= 0) y y) (if (= y 0) (+ 10) y) (if (= y 0) y (+ 10)))")

    //code 3: identifier in set!
    val programText3: String =
      """(let
        |    ((x 5)
        |     (y 9))
        |  (set! y (+ x 10))
        |  (set! x (+ y y)))""".stripMargin

    testLet(programText3, "(let ((y 9)) (set! y (+ 10)) (+ y y))")

    //code 4: identifier in define
    val programText4: String =
      """(let
        |    ((x 5))
        |  (define y (+ 10 (+ x 10))))""".stripMargin

    testLet(programText4, "(let () (define y (+ 10 (+ 10))))")

    //code 5: identifier in let/letStar/letrec
    val programText5: String =
      """(let
        |    ((x 5))
        |  (let ((y x)) 10)
        |  (let* ((z 10)) x))""".stripMargin

    testLet(programText5, "(let () (let () 10))")

    //code 6: identifier in lambda
    val programText6: String =
      """(let
        |    ((x 5))
        |  (lambda (y z) (+ 100 (+ 100 x)))
        |  )""".stripMargin

    testLet(programText6, "(let () (lambda (y z) (+ 100 (+ 100))))")

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


    testLet(programText7, "(let ((a 10) (b 100)) (lambda (y z) (+ 100 (+ 100))) (+ a b (* a 10)) (if a a a) (if a (+ b b) (* b b)) (set! a b))")

    //code 8: Let with more than 1 binding
    val programText8: String =
      """(let
        |    ((x 5)
        |     (y (+ 10 x)))
        |  (* y x)
        |  (begin 10 (+ y 10)))
        |""".stripMargin

    testLet(programText8, "(let ((y (+ 10))) (* y) (begin 10 (+ y 10)))")

    //code 9: Nested let, with nested binding that should be recursively deleted
    val programText9: String =
      """(let ((x 5))
        |  (let ((y x))
        |    (* y x)
        |    (begin 10 (+ y x 10))
        |    )
        |  )""".stripMargin

    testLet(programText9, "(let () (let () (*) (begin 10 (+ 10))))")

    //code 10: if that should be turned into a begin
    val programText10: String =
      """(let ((x 5))
        |  (if x 10 20)
        |  )""".stripMargin

    testLet(programText10, "(let () (begin 10 20))")

    //code 10: a lambda
    val programText11: String =
      """(lambda (a b c)
        |  (+ b c (* a a))
        |  (let ((z b))
        |    c)
        |  )""".stripMargin

    testLambda(programText11, "(lambda (b c) (+ b c (*)) (let ((z b)) c))")

    //code 10: a lambda
    val programText12: String =
      """(lambda (a b c)
        |  (+ b c (* a a))
        |  )""".stripMargin

    testLambda(programText12, "(lambda (b c) (+ b c (*)))")

    //code 11:
    val programText13: String =
      """(lambda (a b c)
        |  (let ((x a)
        |        (y 10))
        |    (+ a x)
        |    y))""".stripMargin

    testLambda(programText13, "(lambda (b c) (let ((y 10)) (+) y))")
  }
}

