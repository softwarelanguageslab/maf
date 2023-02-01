package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeLet.LetIdentifierDeepDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}

class LetIdentifierDeepDropTest extends AnyFlatSpecTransformations {
  "LetIdentifierDeepDrop" should "deep drop a lets identifier" in {
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

    suggestedTrees = LetIdentifierDeepDrop.transform(t, letExp).toList

    assertTreeString("(begin (+ 2 2) (let ((a 10)) (+ a) (if #t a 99)))")

    assertTreeString( "(begin (+ 2 2) (let ((b 100)) b (+ b) (begin #t 99)))")
  }

  "LetIdentifierDeepDrop" should "return empty list for non-let exps" in {
    val programText =
      """(begin
        |  (+ 2 2)
        |  (lambda (a)
        |    (+ 99 a)
        |    (if #t a 99)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val lambdaExp = t.exps(1)

    suggestedTrees = LetIdentifierDeepDrop.transform(t, lambdaExp).toList

    assert(suggestedTrees equals List())
  }
}
