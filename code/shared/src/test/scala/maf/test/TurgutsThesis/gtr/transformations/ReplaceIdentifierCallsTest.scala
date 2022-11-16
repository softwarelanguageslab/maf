package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifierCalls
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class ReplaceIdentifierCallsTest extends AnyFlatSpecTransformations {
  "ReplaceIdentifierCalls" should "replace calls to an identifier with all values" in {
    val programText =
      """(begin
        |  (let ((a 1)
        |        (b 2))
        |    (+ a b)
        |    (a)
        |    (b)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.last

    suggestedTrees = ReplaceIdentifierCalls.transform(t, letExp)
    assert(suggestedTrees.length == 10)

    //replace calls to b
    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) (a) 'S))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) (a) \"S\"))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) (a) #t))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) (a) #f))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) (a) 1))")

    //replace calls to a
    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) 'S (b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) \"S\" (b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) #t (b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) #f (b)))")

    assertTreeString("(begin (let ((a 1) (b 2)) (+ a b) 1 (b)))")
  }

  "ReplaceIdentifierCalls" should "return empty list if there are no identifier calls to replace" in {
    val programText =
      """(begin
        |  (+ 100 100)
        |  (* 100 100))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val appl = t.exps.last

    suggestedTrees = ReplaceIdentifierCalls.transform(t, appl)

    assert(suggestedTrees equals List())
  }
}