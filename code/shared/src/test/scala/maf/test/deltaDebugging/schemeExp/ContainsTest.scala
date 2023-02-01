package maf.test.deltaDebugging.schemeExp

import maf.language.scheme.{SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class ContainsTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to tell when it contains a node" in {
    val programText1: String =
      """(let ((a 10)
        |      (b 100))
        |  (if #t #t #t)
        |  (begin
        |    (begin
        |      (+ a b))))""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]
    val copy = letExp.sexpCopy()

    //NOTE: contains checks for reference equality (e.g. same addr in the heap)
    assert(!letExp.contains(copy))
    assert(!letExp.contains(copy.body.head))

    assert(letExp.contains(letExp))
    assert(letExp.contains(letExp.bindings.head._2))
    assert(letExp.contains(letExp.body.head))
  }
}
