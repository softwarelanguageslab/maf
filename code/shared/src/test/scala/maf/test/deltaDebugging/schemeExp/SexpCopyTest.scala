package maf.test.deltaDebugging.schemeExp

import maf.language.scheme.{SchemeIf, SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class SexpCopyTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to sexpCopy itself" in {
    val programTextIf: String =
      """(if #t (+ 5 5) (+ 10 10))""".stripMargin

    val ifExp: SchemeIf = SchemeParser.parseProgramText(programTextIf).last.asInstanceOf[SchemeIf]
    val ifCopy = ifExp.sexpCopy()

    assert(ifCopy equals ifExp)
    assert(!(ifCopy eq ifExp))
    assert(!ifExp.allSubexpressions.exists(subExp => subExp eq ifCopy.subexpressions.head))

    val programTextLet: String =
      """(let ((a 1))
        | (* a 2)
        | (+ a 10)
        | 100)""".stripMargin
    val letExp: SchemeLet = SchemeParser.parseProgramText(programTextLet).last.asInstanceOf[SchemeLet]
    val letCopy: SchemeLet = letExp.sexpCopy()

    assert(letExp equals letCopy)
    assert(!(letExp eq letCopy))

    assert(letExp.bindings.head._2 equals letCopy.bindings.head._2)
    assert(!(letExp.bindings.head._2 eq letCopy.bindings.head._2))
  }
}
