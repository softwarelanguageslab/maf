package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.IfToBegin
import maf.language.scheme.{SchemeBegin, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class IfToBeginTest extends AnyFlatSpec {
  "IfToBegin" should "suggest begins for an if" in {
    val programText: String =
      """(begin
        |  (if (> 1 2) 'then-case 'else-case)
        |  10)""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]

    val suggestedTrees = IfToBegin.transform(t, t.exps.head)

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (begin (> 1 2) 'then-case) 10)"
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(begin (begin (> 1 2) 'else-case) 10)"
    }))
  }

  "IfToBegin" should "return empty list for a non-if expression" in {
    val programText: String =
      """(begin
        |  (if (> 1 2) 'then-case 'else-case)
        |  10)""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]

    val suggestedTrees = IfToBegin.transform(t, t.exps(1)) //corresponds to the 10

    assert(suggestedTrees equals List())
  }
}
