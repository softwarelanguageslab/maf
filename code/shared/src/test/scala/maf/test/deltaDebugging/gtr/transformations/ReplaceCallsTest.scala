package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.treeDD.transformations.schemeLambda.ReplaceCalls
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeParser}

class ReplaceCallsTest extends AnyFlatSpecTransformations:
  "ReplaceCalls" should "replace the calls to define-bound lambda" in {
    val programText: String =
      """(begin
        |  (define (f x) (* x x))
        |  (f 1)
        |  (+ 2 2)
        |  (f 111))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val defineExp = t.exps.head

    val suggestedTrees = ReplaceCalls.transform(t, defineExp).toList //should remove calls to f

    assert(suggestedTrees.length > 1)
    assertTreeString("(begin #f (+ 2 2) #f)", suggestedTrees)
    assertTreeString("(begin 1 (+ 2 2) 1)", suggestedTrees)
    assertTreeString("(begin 'S (+ 2 2) 'S)", suggestedTrees)
  }

  "ReplaceCalls" should "remove the calls to let-bound lambda" in {
    val programText: String =
      """(begin
        |  (let ((f (lambda (x) (* x x))))
        |    (f 10)
        |    (f 100)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    val suggestedTrees = ReplaceCalls.transform(t, letExp).toList //should remove calls to f
    assert(suggestedTrees.length > 1)
    assertTreeString("(begin (let () #t #t 1000) (+ 2 2))", suggestedTrees)
    assertTreeString("(begin (let () 1 1 1000) (+ 2 2))", suggestedTrees)
    assertTreeString("(begin (let () 'S 'S 1000) (+ 2 2))", suggestedTrees)
  }

  "ReplaceCalls" should "return an empty list given a non-lambda-binding exp" in {
    val programText: String =
      """(begin
        |  (let ((f 100))
        |    (+ f f)
        |    1000)
        |  (+ 2 2))
        |  """.stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head

    val suggestedTrees = ReplaceCalls.transform(t, letExp).toList
    assert(suggestedTrees equals List())
  }