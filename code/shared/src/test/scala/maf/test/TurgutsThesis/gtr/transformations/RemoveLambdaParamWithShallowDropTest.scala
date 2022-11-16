package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.RemoveLambdaParamWithShallowDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveLambdaParamWithShallowDropTest extends AnyFlatSpec {
  "RemoveLambdaParamWithShallowDrop" should "shallow drop a lambdas param" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    val suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, defineExp)
    println(suggestedTrees)
    assert(suggestedTrees.length == 2)
    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (define f (lambda (x) (* x x))) (f 1) (f 111))"
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (define f (lambda (y) (* y y))) (f 2) (f 222))"
    }))
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

    val suggestedTrees = RemoveLambdaParamWithShallowDrop.transform(t, fAppl)
    assert(suggestedTrees equals List())
  }
}
