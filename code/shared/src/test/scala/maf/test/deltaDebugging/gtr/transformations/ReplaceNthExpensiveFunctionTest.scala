package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeLambda.ReplaceNthExpensiveFunction
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

    val suggestedTrees = transformation.transform(tree, let).toList

    assertTreeString("(begin (let ((a 1)) (+ a 1) 1 a) 10)", suggestedTrees)
  }

  "ReplaceNthExpensiveFunction" should "replace calls to the correct function and remove it, def-exp" in {
    val programText: String =
      """(begin
        |  (define (square x)
        |    (* x x))
        |  (square 5)
        |  (map square '(1 2 3)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    val transformation = new ReplaceNthExpensiveFunction(Array(("square", 100)), 0)

    val suggestedTrees = transformation.transform(t, defineExp).toList //should remove calls to f

    assertTreeString("(begin #t (map #t (__toplevel_cons 1 (__toplevel_cons 2 (__toplevel_cons 3 ())))))", suggestedTrees)
  }

