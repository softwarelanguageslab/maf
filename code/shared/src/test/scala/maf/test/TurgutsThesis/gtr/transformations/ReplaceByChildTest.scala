package maf.test.TurgutsThesis.gtr.transformations
import maf.TurgutsThesis.gtr.transformations.ReplaceByChild
import maf.language.scheme.{SchemeBegin, SchemeFuncall, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class ReplaceByChildTest extends AnyFlatSpec {
  "ReplaceByChild" should "suggest a begins all children" in {
    val programText: String =
      """(+
        |  (begin (* 1 1) (+ 100 100))
        |  9999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]

    val suggestedTrees = ReplaceByChild.transform(t, t.args.head)

    assert(suggestedTrees.length == 2)
    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(+ (+ 100 100) 9999)" //begin replaced by (+ 100 100)
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "(+ (* 1 1) 9999)" //begin replaced by (* 1 1)
    }))
  }

  "ReplaceByChild" should "be able to replace an application by its children" in {
    val programText: String =
      """(+ 1 999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]

    val suggestedTrees = ReplaceByChild.transform(t, t)

    assert(suggestedTrees.length == 3)

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "999"
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "1"
    }))

    assert(suggestedTrees.exists(tree => {
      tree.toString equals "+"
    }))

  }

  "ReplaceByChild" should "return an empty list if there is no child to replace with" in {
    val programText: String =
      """(+ 1 999)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]

    val suggestedTrees = ReplaceByChild.transform(t, t.args.head)

    assert(suggestedTrees equals List())
  }
}
