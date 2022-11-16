package maf.test.TurgutsThesis.gtr.transformations

import org.scalatest.flatspec.AnyFlatSpec
import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifier
import maf.language.scheme.{SchemeBegin, SchemeParser}

class ReplaceIdentifierTest extends AnyFlatSpecTransformations {
  "ReplaceIdentifier" should "replace an identifier with all values" in {
    val programText =
      """(begin
        |  (let ((a 1)
        |        (b 2))
        |    (+ a b)
        |    (* a b)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.last

    suggestedTrees = ReplaceIdentifier.transform(t, letExp)
    assert(suggestedTrees.length == 10)

    //replace b
    assertTreeString("(begin (let ((a 1) (b 2)) (+ a 'S) (* a 'S)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a \"S\") (* a \"S\")))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a #t) (* a #t)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a #f) (* a #f)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a 1) (* a 1)))")

    //replace a
    assertTreeString("(begin (let ((a 1) (b 2)) (+ 'S b) (* 'S b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ \"S\" b) (* \"S\" b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ #t b) (* #t b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ #f b) (* #f b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ 1 b) (* 1 b)))")
  }

  "ReplaceIdentifier" should "return empty list if there are no identifiers to replace" in {
    val programText =
      """(begin
        |  (+ 100 100)
        |  (* 100 100))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val appl = t.exps.last

    suggestedTrees = ReplaceIdentifier.transform(t, appl)

    assert(suggestedTrees equals List())
  }
}
