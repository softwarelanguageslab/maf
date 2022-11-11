package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLet, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import org.scalatest.flatspec.AnyFlatSpec

class DeleteChildrenTest extends AnyFlatSpec {
  "A SchemeExp" should "be able to delete its children" in {

    def testCode(programText: String, deleter: SchemeExp => Boolean): Unit = {
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]

      println(letExp.deleteChildren(deleter))
    }

    val programText: String =
      """(let ((x 5)
        |      (y (+ 10 x)))
        |  (* y x)
        |  (if x 5 x)
        |  (x)
        |  (begin 10 (+ y 10)))
        |""".stripMargin
    
    testCode(programText, sexp => {
      sexp match
        case exp: SchemeVarExp =>
          exp.id.name == "x"
        case _ => false
    })
  }


  "A SchemeExp" should "return None if it must be deleted" in {
    val programText: String =
      """(let ((x 5)
        |      (y (+ 10 x)))
        |  x)
        |""".stripMargin

    val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]

    val deleted = letExp.deleteChildren(exp =>
      exp match
        case s: SchemeVarExp => true
        case _ => false
    )

    assert(deleted.isEmpty)
  }
}
