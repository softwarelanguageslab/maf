package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
import maf.language.scheme.{SchemeBegin, SchemeLet, SchemeParser}

class ReplaceNthExpensiveFunctionTest extends AnyFlatSpecTransformations:
  "ReplaceNthExpensiveFunction" should "replace calls to the correct function and remove it" in {
    val programText =
      """(begin
        |  (let ((a 1)
        |        (f (lambda (x y) (+ x y))))
        |    (+ a (f 2 2))
        |    (f 3 3)
        |    a)
        |  10)""".stripMargin

    val tree: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val let: SchemeLet = tree.exps.head.asInstanceOf[SchemeLet]

    val transformation = new ReplaceNthExpensiveFunction(Array(("f", 100)), 0)

    suggestedTrees = transformation.transform(tree, let)

    assertTreeString("(begin (let ((a 1)) (+ a 1) 1 a) 10)")
  }

