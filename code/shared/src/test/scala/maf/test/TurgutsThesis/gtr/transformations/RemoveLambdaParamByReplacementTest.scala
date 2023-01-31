package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeLambda.RemoveLambdaParamByReplacement
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class RemoveLambdaParamByReplacementTest extends AnyFlatSpecTransformations {
  "RemoveLambdaParamByReplacement" should "remove a lambda's param and replace it with all values" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head
    suggestedTrees = RemoveLambdaParamByReplacement.transform(t, defineExp)
    assert(suggestedTrees.length > 10)

    suggestedTrees.foreach(t => println(t.prettyString()))

    //remove param y
    assertTreeString("(begin (define f (lambda (x) (* x x) (* 'S 'S))) (f 1) (f 111))")

    assertTreeString("(begin (define f (lambda (x) (* x x) (* \"S\" \"S\"))) (f 1) (f 111))")

    assertTreeString("(begin (define f (lambda (x) (* x x) (* #f #f))) (f 1) (f 111))")

    assertTreeString("(begin (define f (lambda (x) (* x x) (* #t #t))) (f 1) (f 111))")

    assertTreeString("(begin (define f (lambda (x) (* x x) (* 1 1))) (f 1) (f 111))")

    //remove param x
    assertTreeString("(begin (define f (lambda (y) (* 'S 'S) (* y y))) (f 2) (f 222))")

    assertTreeString("(begin (define f (lambda (y) (* \"S\" \"S\") (* y y))) (f 2) (f 222))")

    assertTreeString("(begin (define f (lambda (y) (* #f #f) (* y y))) (f 2) (f 222))")

    assertTreeString("(begin (define f (lambda (y) (* #t #t) (* y y))) (f 2) (f 222))")

    assertTreeString("(begin (define f (lambda (y) (* 1 1) (* y y))) (f 2) (f 222))")
  }

  "RemoveLambdaParamByReplacement" should "return an empty list given non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (define (f x y) (* x x) (* y y))
        |  (f 1 2)
        |  (f 111 222))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val fApplication = t.exps.last

    suggestedTrees = RemoveLambdaParamByReplacement.transform(t, fApplication)
    assert(suggestedTrees equals List())
  }
}
