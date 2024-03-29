package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeLambda.RemoveLambdaParamWithShallowDrop
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

    val suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, defineExp).toList
    
    assert(suggestedTrees.length == 2)
    assertTreeString("(begin (define f (lambda (x) (* x x))) (f 1) (f 111))", suggestedTrees)

    assertTreeString("(begin (define f (lambda (y) (* y y))) (f 2) (f 222))", suggestedTrees)
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

    val suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, fAppl).toList
    assert(suggestedTrees equals List())
  }
}
