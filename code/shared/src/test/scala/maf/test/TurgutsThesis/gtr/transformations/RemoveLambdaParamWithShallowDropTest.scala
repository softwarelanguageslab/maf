package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeLambda.RemoveLambdaParamWithShallowDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveLambdaParamWithShallowDropTest extends AnyFlatSpecTransformations {
  "RemoveLambdaParamWithShallowDrop" should "shallow drop a lambdas param" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, defineExp).toList
    
    assert(suggestedTrees.length == 2)
    assertTreeString("(begin (define f (lambda (x) (* x x))) (f 1) (f 111))")

    assertTreeString("(begin (define f (lambda (y) (* y y))) (f 2) (f 222))")
  }

  "RemoveLambdaParamWithShallowDrop" should "return empty list given a non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val fAppl = t.exps.last

    suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, fAppl).toList
    assert(suggestedTrees equals List())
  }
}
