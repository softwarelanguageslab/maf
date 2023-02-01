package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeLambda.RemoveCallsAndReplaceByBody
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveCallsAndReplaceByBodyTest extends AnyFlatSpecTransformations {
  "RemoveCallsAndReplaceByBody" should "remove the calls to define-bound lambda, and replace lambda by body" in {
    val programText: String =
      """(begin
        |  (define (f x) (* x x))
        |  (f 1)
        |  (+ 2 2)
        |  (f 111))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    suggestedTrees = RemoveCallsAndReplaceByBody.transform(t, defineExp).toList //should remove calls to f

    assert(suggestedTrees.length == 1)
    assertTreeString("(begin (define f (begin (* x x))) (+ 2 2))")
  }

  "RemoveCallsAndReplaceByBody" should "remove the calls to let-bound lambda, and replace the lambda by body" in {
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

    suggestedTrees = RemoveCallsAndReplaceByBody.transform(t, letExp).toList //should remove calls to f
    assert(suggestedTrees.length == 1)
    assertTreeString("(begin (let ((f (begin (* x x)))) 1000) (+ 2 2))")
  }

  "RemoveCallsAndReplaceByBody" should "return an empty list given a non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (let ((f 100))
        |    (+ f f)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    suggestedTrees = RemoveCallsAndReplaceByBody.transform(t, letExp).toList
    assert(suggestedTrees equals List())
  }
}
