package maf.test.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.generics.ReplaceByValue
import maf.deltaDebugging.gtr.transformations.schemeIdentifier.ReplaceIdentifier
import org.scalatest.flatspec.AnyFlatSpec
import maf.language.scheme.{SchemeBegin, SchemeFuncall, SchemeLet, SchemeParser}

class ReplaceIdentifierTest extends AnyFlatSpecTransformations {
  "ReplaceByValue" should "replace an identifier with all values" in {
    val programText =
      """(begin
        |  (let ((a 1)
        |        (b 2))
        |    (+ a b)
        |    (* a b))
        |  10)""".stripMargin

    val t: SchemeBegin = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeBegin]
    val letExp = t.exps.head.asInstanceOf[SchemeLet]
    val varExp = letExp.body.head.asInstanceOf[SchemeFuncall].args.head

    suggestedTrees = ReplaceIdentifier.transform(t, varExp).toList
    assertTreeString("(begin (let ((a 1) (b 2)) (+ 1 b) (* a b)) 10)")
  }
}
