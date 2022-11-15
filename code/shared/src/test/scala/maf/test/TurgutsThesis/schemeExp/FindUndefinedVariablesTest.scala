package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class FindUndefinedVariablesTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to find its undefined vars" in {
    val programText1: String =
      """(let ((a 10)
        |      (b 100))
        |  (+ a b (* a 10))
        |  (if a a a)
        |  (if a (+ b b) (* b b))
        |  (set! a b))""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]

    assert(letExp.findUndefinedVariables() equals List())


    val programText2: String =
      """(let ((a 10))
        |  (+ a b (* a 10))
        |  (* c d)
        |  (set! a b))""".stripMargin

    val letExp2: SchemeLet = SchemeParser.parseProgramText(programText2).last.asInstanceOf[SchemeLet]

    assert(letExp2.findUndefinedVariables().exists(id => id.name equals "d"))
    assert(letExp2.findUndefinedVariables().exists(id => id.name equals "c"))
    assert(letExp2.findUndefinedVariables().exists(id => id.name equals "b"))
  }
}
