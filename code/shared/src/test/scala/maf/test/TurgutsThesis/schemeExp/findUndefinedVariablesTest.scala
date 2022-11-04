package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class findUndefinedVariablesTest extends AnyFlatSpec {
  "A scheme Exp" should "be able to find its undefined vars" in {

    val programText1: String =
      "(let " +
        "(" +
          "(a 10)" +
          "(b 100))" +
           "(+ a b (* a 10))" +
           "(if a a a)" +
           "(if a (+ b b) (* b b))" +
           "(set! a b)" +
        ")"

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]

    assert(letExp.findUndefinedVariables() equals List())
  }
}
