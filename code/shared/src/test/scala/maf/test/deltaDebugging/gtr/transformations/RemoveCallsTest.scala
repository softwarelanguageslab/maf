package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.treeDD.transformations.schemeLambda.RemoveCalls
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveCallsTest extends AnyFlatSpecTransformations {
  "RemoveCalls" should "remove the calls to define-bound lambda" in {
    val programText: String =
      """(begin
        |  (define (f x) (* x x))
        |  (f 1)
        |  (+ 2 2)
        |  (f 111))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    val suggestedTrees = RemoveCalls.transform(t, defineExp).toList //should remove calls to f

    assertTreeString("(begin (+ 2 2))", suggestedTrees)
  }

  "RemoveCalls" should "remove the calls to let-bound lambda" in {
    val programText: String =
      """(begin
        |  (let ((f (lambda (x) (* x x))))
        |    (f 10)
        |    (f 100)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    val suggestedTrees = RemoveCalls.transform(t, letExp).toList //should remove calls to f
    assertTreeString("(begin (let () 1000) (+ 2 2))", suggestedTrees)
  }

  "RemoveCalls" should "return an empty list given a non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (let ((f 100))
        |    (+ f f)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    val suggestedTrees = RemoveCalls.transform(t, letExp).toList
    assert(suggestedTrees equals List())
  }
}
