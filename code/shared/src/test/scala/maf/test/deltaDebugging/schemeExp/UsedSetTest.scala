package maf.test.deltaDebugging.schemeExp

import maf.language.scheme.{SchemeLet, SchemeParser}
import org.scalatest.flatspec.AnyFlatSpec

class UsedSetTest extends AnyFlatSpec {
  
  "A SchemeExp" should "be able to find its used set" in {
    val programText1: String =
      """(let ((a 10))
        |  (+ a b (* a 10))
        |  (* c d)
        |  (set! a b))""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText1).last.asInstanceOf[SchemeLet]

    assert(letExp.usedSet().exists(identifier => identifier.name equals "b"))
    assert(letExp.usedSet().exists(identifier => identifier.name equals "c"))
    assert(letExp.usedSet().exists(identifier => identifier.name equals "d"))
  }
}
