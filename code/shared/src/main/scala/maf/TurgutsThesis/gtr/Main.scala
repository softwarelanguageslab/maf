package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.{substituteByChild, deleteChild}
import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLetrec, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object Main {
  def main(args: Array[String]): Unit = {
    val programText: String =
      "(begin " +
        "(if (= x 5) #t #f)" +
        "(begin (+ x 2) (* 100 100) (if #f #f #f)))"

    val t = SchemeParser.parseProgramText(programText).last

    println(t)
    val reduced = GTR.reduce(t,
      t => t.isInstanceOf[SchemeBegin] && t.asInstanceOf[SchemeBegin].exps.length == 1,
      List(substituteByChild, deleteChild))

    println(reduced)
  }
}
