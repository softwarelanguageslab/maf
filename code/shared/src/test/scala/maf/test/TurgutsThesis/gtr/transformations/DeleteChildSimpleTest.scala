package maf.test.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.DeleteChildSimple
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambdaExp, SchemeLettishExp, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class DeleteChildSimpleTest extends AnyFlatSpecTransformations {
  "DeleteChildSimple" should "correctly reduce lambdas" in {
    val programText: String =
      """(begin
        |  (+ 10 10)
        |  (lambda (x) (+ x x) (* x x)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val lambdaExp = t.exps(1)

    suggestedTrees = DeleteChildSimple.transform(t, lambdaExp)

    assert(suggestedTrees.length == 2)
    assert(!(suggestedTrees.head eq t)) //should return a new tree
    assert(!(suggestedTrees(1) eq t))

    checkSuggestedTreeString("(begin (+ 10 10) (lambda (x) (+ x x)))")

    checkSuggestedTreeString("(begin (+ 10 10) (lambda (x) (* x x)))")
  }

  "DeleteChildSimple" should "correctly reduce lettishExps" in {
    val programText: String =
      """(begin
        |  (+ 10 10)
        |  (let ((a 10))
        |    (+ a a)
        |    (* a a)))""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps(1)

    suggestedTrees = DeleteChildSimple.transform(t, letExp)
    
    checkSuggestedTreeString("(begin (+ 10 10) (let () (+ a a) (* a a)))") //binding reduced

    checkSuggestedTreeString("(begin (+ 10 10) (let ((a 10)) (+ a a)))") //multiplication removed

    checkSuggestedTreeString("(begin (+ 10 10) (let ((a 10)) (* a a)))") //addition removed
  }

  "DeleteChildSimple" should "return an empty list given a SchemeExp without a child" in {
    val programText: String =
      """(+ 10 10)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]
    val numberExp = t.args.head

    suggestedTrees = DeleteChildSimple.transform(t, numberExp)

    assert(suggestedTrees equals List())
  }

  "DeleteChildSimple" should "return an empty list given a begin with only 1 expr" in {
    val programText: String =
      """(+ (begin 10) 10)""".stripMargin

    val t: SchemeFuncall = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeFuncall]
    val begin = t.args.head

    suggestedTrees = DeleteChildSimple.transform(t, begin)

    assert(suggestedTrees equals List())
  }
}
