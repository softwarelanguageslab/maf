package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeLet.LetIdentifierShallowDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class LetIdentifierShallowDropTest extends AnyFlatSpecTransformations {
  "LetIdentifierShallowDrop" should "deep drop a lets identifier" in {
    val programText =
      """(begin
        |  (+ 2 2)
        |  (let ((a 10)
        |        (b 100))
        |    b
        |    (+ b a)
        |    (if #t a 99)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps(1)

    val suggestedTrees = LetIdentifierShallowDrop.transform(t, letExp).toList

    assertTreeString("(begin (+ 2 2) (let ((a 10)) (if #t a 99)))", suggestedTrees)

    assertTreeString("(begin (+ 2 2) (let ((b 100)) b))", suggestedTrees)
  }

  "LetIdentifierShallowDrop" should "return empty list for non-let exps" in {
    val programText =
      """(begin
        |  (+ 2 2)
        |  (lambda (a)
        |    (+ 99 a)
        |    (if #t a 99)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val lambdaExp = t.exps(1)

    val suggestedTrees = LetIdentifierShallowDrop.transform(t, lambdaExp).toList

    assert(suggestedTrees equals List())
  }
}

