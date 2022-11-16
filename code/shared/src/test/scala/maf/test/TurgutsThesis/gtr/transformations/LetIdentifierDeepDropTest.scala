package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.LetIdentifierDeepDrop
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class LetIdentifierDeepDropTest extends AnyFlatSpec {
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

    val suggestedTrees = LetIdentifierDeepDrop.transform(t, letExp)

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (+ 2 2) (let ((a 10)) (+ a) (if #t a 99)))"
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (+ 2 2) (let ((b 100)) b (+ b) (begin #t 99)))"
    }))
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

    val suggestedTrees = LetIdentifierDeepDrop.transform(t, lambdaExp)

    assert(suggestedTrees equals List())
  }
}
