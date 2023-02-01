package maf.test.deltaDebugging.schemeExp

import maf.language.scheme.{SchemeBegin, SchemeIf, SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class LevelNodesTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to find its nodes at a certain level" in {
    val programText1: String =
      """(let ((a 10)
        |      (b 100))
        |  (if #t #t #t)
        |  (begin
        |    (begin
        |      (+ a b))))""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]

    //level 0:
    assert(letExp.levelNodes(0).length == 1)
    assert(letExp.levelNodes(0).head eq letExp)

    //level 1:
    assert(letExp.levelNodes(1).length == 4)
    assert(letExp.levelNodes(1).exists(subExp => subExp eq letExp.bindings.head._2))
    assert(letExp.levelNodes(1).exists(subExp => subExp eq letExp.bindings(1)._2))
    assert(letExp.levelNodes(1).exists(subExp => subExp eq letExp.body.head))
    assert(letExp.levelNodes(1).exists(subExp => subExp eq letExp.body(1)))

    //level 2:
    assert(letExp.levelNodes(2).length == 4)
    assert(letExp.levelNodes(2).exists(subExp => subExp eq letExp.body.head.asInstanceOf[SchemeIf].cond))
    assert(letExp.levelNodes(2).exists(subExp => subExp eq letExp.body.head.asInstanceOf[SchemeIf].cons))
    assert(letExp.levelNodes(2).exists(subExp => subExp eq letExp.body.head.asInstanceOf[SchemeIf].alt))
    assert(letExp.levelNodes(2).exists(subExp => subExp eq letExp.body(1).asInstanceOf[SchemeBegin].exps.head))
  }
}
