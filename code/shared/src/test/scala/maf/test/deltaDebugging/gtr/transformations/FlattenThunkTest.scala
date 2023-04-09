package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeLambda.FlattenThunk
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class FlattenThunkTest extends AnyFlatSpecTransformations {
  "FlattenThunk" should "flatten defined thunks" in {
    val programText: String =
      """(begin
        |  (define (f) (* 10 10))
        |  (f)
        |  (+ 2 2)
        |  (f))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    val suggestedTrees = FlattenThunk.transform(t, defineExp).toList //should remove calls to f

    assert(suggestedTrees.length == 1)
    assertTreeString("(begin (define f (begin (* 10 10))) f (+ 2 2) f)", suggestedTrees)
  }

  "FlattenThunk" should "flatten let-bound thunks" in {
    val programText: String =
      """(begin
        |  (let ((f (lambda () (* 10 10))))
        |    (f)
        |    (f)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    val suggestedTrees = FlattenThunk.transform(t, letExp).toList //should remove calls to f

    assert(suggestedTrees.length == 1)
    assertTreeString("(begin (let ((f (begin (* 10 10)))) f f 1000) (+ 2 2))", suggestedTrees)
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

    val suggestedTrees = FlattenThunk.transform(t, letExp).toList
    assert(suggestedTrees equals List())
  }
}
