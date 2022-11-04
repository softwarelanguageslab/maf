package maf.test.TurgutsThesis.gtr

import maf.language.scheme.*
import org.scalatest.flatspec.AnyFlatSpec

class deepDeleteIdentifierTest extends AnyFlatSpec {

  "GTR" should "be able to deep delete identifiers" in {
    def testCode(programText: String, expected: String = "(let () )"): Unit =
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
      val id = letExp.bindings.head._1
      val deepDeleted = letExp.deepDeleteIdentifier(id)

      assert(deepDeleted.toString == expected)

    //code 1: identifier in appl
    val programText1: String =
      "(let " +
        "((x 5))" +
        "(+ y x)" +
        "(x y y)" +
        "(x x x))"

    testCode(programText1, "(let () (+ y) (y y))")

    //code 2: identifier in if
    val programText2: String =
      "(let " +
        "((x 5))" +
        "(if (= x 0) y y)" +
        "(if (= y 0) (+ x 10) y)" +
        "(if (= y 0) y (+ x 10)))"

    testCode(programText2, "(let () (if (= 0) y y) (if (= y 0) (+ 10) y) (if (= y 0) y (+ 10)))")

    //code 3: identifier in set!
    val programText3: String =
      "(let " +
        "((x 5))" +
        "(set! y (+ x 10))" +
        "(set! x (+ y y)))"

    testCode(programText3, "(let () (set! y (+ 10)) (+ y y))")

    //code 4: identifier in define
    val programText4: String =
      "(let " +
        "((x 5))" +
        "(define y (+ 10 (+ x 10))))"

    testCode(programText4, "(let () (define y (+ 10 (+ 10))))")

    //code 5: identifier in let/let*/letrec
    val programText5: String =
      "(let " +
        "((x 5))" +
        "(let ((y x)) 10)" +
        "(let* ((z (+ x 10))) (+ x 10))" +
      ")"

    testCode(programText5, "(let () (let () 10) (let* ((z (+ 10))) (+ 10)))")

    //code 6: identifier in lambda
    val programText6: String =
      "(let " +
        "((x 5))" +
        "(lambda (y z) (+ 100 (+ 100 x)))" +
        ")"

    testCode(programText6, "(let () (lambda (y z) (+ 100 (+ 100))))")

    //code 7: identifier should not be deleted everywhere

    val programText7: String =
      "(let " +
        "((x 5))" +
        "(lambda (y z) (+ 100 (+ 100 x)))" +
        "(+ z y (* y 10))" +
        "(if a b c)" +
        "(if a (+ b b) (+ c c))" +
        "(set! a c)" +
        ")"

    testCode(programText7, "(let () (lambda (y z) (+ 100 (+ 100))) (+ z y (* y 10)) (if a b c) (if a (+ b b) (+ c c)) (set! a c))")
  }
}
