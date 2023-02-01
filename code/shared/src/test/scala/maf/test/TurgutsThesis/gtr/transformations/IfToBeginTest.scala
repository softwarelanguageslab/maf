package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeIf.IfToBegin
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class IfToBeginTest extends AnyFlatSpecTransformations {
  "IfToBegin" should "suggest begins for an if" in {
    val programText: String =
      """(begin
        |  (if (> 1 2) 'then-case 'else-case)
        |  10)""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val ifExp = t.exps.head

    suggestedTrees = IfToBegin.transform(t, ifExp).toList

    assertTreeString("(begin (begin (> 1 2) 'then-case) 10)")

    assertTreeString("(begin (begin (> 1 2) 'else-case) 10)")
  }

  "IfToBegin" should "return empty list for a non-if expression" in {
    val programText: String =
      """(begin
        |  (if (> 1 2) 'then-case 'else-case)
        |  10)""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val numberExp = t.exps(1) //corresponds to 10

    suggestedTrees = IfToBegin.transform(t, numberExp).toList

    assert(suggestedTrees equals List())
  }
}
