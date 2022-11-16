package maf.test.TurgutsThesis.gtr.transformations

import maf.language.scheme.{SchemeBegin, SchemeParser}
import maf.TurgutsThesis.gtr.transformations.RemoveCalls
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

    suggestedTrees = RemoveCalls.transform(t, defineExp) //should remove calls to f

    assert(suggestedTrees.length == 1)
    checkSuggestedTreeString("(begin (define f (lambda (x) (* x x))) (+ 2 2))")
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

    suggestedTrees = RemoveCalls.transform(t, letExp) //should remove calls to f
    assert(suggestedTrees.length == 1)
    checkSuggestedTreeString("(begin (let ((f (lambda (x) (* x x)))) 1000) (+ 2 2))")
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

    suggestedTrees = RemoveCalls.transform(t, letExp)
    assert(suggestedTrees equals List())
  }
}
