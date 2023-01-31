package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.generics.ReplaceByChild
import maf.language.scheme.{SchemeBegin, SchemeFuncall, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class ReplaceByChildTest extends AnyFlatSpecTransformations {
  "ReplaceByChild" should "suggest a begins all children" in {
    val programText: String =
      """(+
        |  (begin (* 1 1) (+ 100 100))
        |  9999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]
    val beginExp = t.args.head

    suggestedTrees = ReplaceByChild.transform(t, beginExp)

    assert(suggestedTrees.length == 2)
    assertTreeString("(+ (+ 100 100) 9999)") //begin replaced by (+ 100 100)

    assertTreeString("(+ (* 1 1) 9999)") //begin replaced by (* 1 1)
  }

  "ReplaceByChild" should "be able to replace an application by its children" in {
    val programText: String =
      """(+ 1 999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]

    suggestedTrees = ReplaceByChild.transform(t, t)

    assert(suggestedTrees.length == 3)

    assertTreeString("999")

    assertTreeString("1")

    assertTreeString("+")
  }

  "ReplaceByChild" should "return an empty list if there is no child to replace with" in {
    val programText: String =
      """(+ 1 999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]
    val numberExp = t.args.head

    suggestedTrees = ReplaceByChild.transform(t, numberExp)

    assert(suggestedTrees equals List())
  }
}
