package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{SchemeLet, SchemeParser, SchemeVarArgLambda}
import org.scalatest.flatspec.AnyFlatSpec

class DefinedSetTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to find its defined set" in {
    val programText1: String =
      """(let ((a 10)
        |      (b 100)
        |      (c 1000)
        |      (d 10000))
        |  (+ a b (* a 10))
        |  (if a a a)
        |  (if a (+ b b) (* b b))
        |  (set! a b))""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]

    assert(letExp.definedSet().exists(identifier => identifier.name equals "a"))
    assert(letExp.definedSet().exists(identifier => identifier.name equals "b"))
    assert(letExp.definedSet().exists(identifier => identifier.name equals "c"))
    assert(letExp.definedSet().exists(identifier => identifier.name equals "d"))
  }

  "A SchemeVarArg" should "be able to find its defined vars" in {
    val programText1: String =
      """(lambda args
        |  args)""".stripMargin

    val lambdaExp: SchemeVarArgLambda = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeVarArgLambda]
    assert(lambdaExp.definedSet().exists(identifier => identifier.name equals "args"))
  }
}
