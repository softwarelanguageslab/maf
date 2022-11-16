package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.RemoveLambdaParamWithDeepDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveLambdaParamWithDeepDropTest extends AnyFlatSpecTransformations {
  "RemoveLambdaParamWithDeepDrop" should "deep drop a lambdas param" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    suggestedTrees = RemoveLambdaParamWithDeepDrop.transform(t, defineExp)
    assert(suggestedTrees.length == 2)
    checkSuggestedTreeString("(begin (define f (lambda (x) (* x x) (*))) (f 1) (f 111))")

    checkSuggestedTreeString("(begin (define f (lambda (y) (*) (* y y))) (f 2) (f 222))")
  }

  "RemoveLambdaParamWithDeepDrop" should "return empty list given a non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val fAppl = t.exps.last

    suggestedTrees = RemoveLambdaParamWithDeepDrop.transform(t, fAppl)
    assert(suggestedTrees equals List())
  }
}
