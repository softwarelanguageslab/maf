package maf.test.TurgutsThesis.schemeExp

import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLet, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import org.scalatest.flatspec.AnyFlatSpec

class DeleteChildrenTest extends AnyFlatSpec {
  "A Scheme exp" should "be able to delete its children" in {

    def testCode(programText: String, deleter: SchemeExp => Boolean): Unit = {
      val letExp: SchemeLet = SchemeParser.parseProgramText(programText).last.asInstanceOf[SchemeLet]
      letExp.setPaths()

      val newExp = letExp.deleteChildren(deleter).get.asInstanceOf[SchemeLet]
      assert(newExp.toString equals "(let ((x 5) (y (+ 10))) (* y) (begin 10 (+ y 10)))")
      assert(newExp.body(1).path equals List(3))
    }

    val programText8: String =
      """(let ((x 5)
        |      (y (+ 10 x)))
        |  (* y x)
        |  (if x 5 x)
        |  (x)
        |  (begin 10 (+ y 10)))
        |""".stripMargin
    
    testCode(programText8, sexp => {
      sexp match
        case exp: SchemeVarExp =>
          exp.id.name == "x"
        case _ => false
    })
  }
}
