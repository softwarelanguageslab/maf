package maf.test.TurgutsThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLet, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value.Real
import org.scalatest.flatspec.AnyFlatSpec
import maf.TurgutsThesis.gtr.transformations.deleteChildLettishExp

class DeepDropIdentifierTest extends AnyFlatSpec {
  "GTR" should "be able to shallow delete identifiers" in {
    def testCode(programText: String, expected: String = "(let () )"): Unit =
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
      val id = letExp.bindings.head._1
      val deepDropped = letExp.deepDropIdentifier(id)

      //println(shallowDropped.toString)
      assert(deepDropped.toString == expected)

    //code 1: identifier in appl
    val programText1: String =
      """(let
        |    ((x 5))
        |  (+ 10 x)
        |  (x 5 9)
        |  (x x x))""".stripMargin

    testCode(programText1, "(let () (+ 10) (5 9))")

    //code 2: identifier in if
    val programText2: String =
      """(let
        |    ((x 5))
        |  (define y 10)
        |  (if (= x 0) y y)
        |  (if (= y 0) (+ x 10) y)
        |  (if (= y 0) y (+ x 10)))""".stripMargin

    testCode(programText2, "(let () (define y 10) (if (= 0) y y) (if (= y 0) (+ 10) y) (if (= y 0) y (+ 10)))")

    //code 3: identifier in set!
    val programText3: String =
      """(let
        |    ((x 5)
        |     (y 9))
        |  (set! y (+ x 10))
        |  (set! x (+ y y)))""".stripMargin

    testCode(programText3, "(let ((y 9)) (set! y (+ 10)) (+ y y))")

    //code 4: identifier in define
    val programText4: String =
      """(let
        |    ((x 5))
        |  (define y (+ 10 (+ x 10))))""".stripMargin

    testCode(programText4, "(let () (define y (+ 10 (+ 10))))")

    //code 5: identifier in let/letStar/letrec
    val programText5: String =
      """(let
        |    ((x 5))
        |  (let ((y x)) 10)
        |  (let* ((z 10)) x))""".stripMargin

    testCode(programText5, "(let () (let () 10) (let* ((z 10)) ))")

    //code 6: identifier in lambda
    val programText6: String =
      """(let
        |    ((x 5))
        |  (lambda (y z) (+ 100 (+ 100 x)))
        |  )""".stripMargin

    testCode(programText6, "(let () (lambda (y z) (+ 100 (+ 100))))")

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


    testCode(programText7, "(let ((a 10) (b 100)) (lambda (y z) (+ 100 (+ 100))) (+ a b (* a 10)) (if a a a) (if a (+ b b) (* b b)) (set! a b))")

    //code 8: Let with more than 1 binding
    val programText8: String =
      """(let
        |    ((x 5)
        |     (y (+ 10 x)))
        |  (* y x)
        |  (begin 10 (+ y 10)))
        |""".stripMargin

    testCode(programText8, "(let ((y (+ 10))) (* y) (begin 10 (+ y 10)))")

    //code 9: Nested let, with nested binding that should be recursively deleted
    val programText9: String =
      """(let ((x 5))
        |  (let ((y x))
        |    (* y x)
        |    (begin 10 (+ y x 10))
        |    )
        |  )""".stripMargin

    testCode(programText9, "(let () (let () (*) (begin 10 (+ 10))))")

    //code 10: if that should be turned into a begin
    val programText10: String =
      """(let ((x 5))
        |  (if x 10 20)
        |  )""".stripMargin

    testCode(programText10, "(let () (begin 10 20))")
  }
}

