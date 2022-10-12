package TurgutThesis.gtr

import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLetrec, SchemeLettishExp, SchemeParser, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object Main {
  def main(args: Array[String]): Unit = {
    val t: SchemeFuncall = SchemeParser.parseProgramText("(* (if 10 5 3) (/ 10 2))").head.asInstanceOf[SchemeFuncall]
    println(t)
    val operator: SchemeExp = t.f
    println(operator)
    val newTree = t.replace(operator, SchemeBegin(List(operator, operator), NoCodeIdentity))
    println(newTree)

    val reduced = GTR.reduce(t, sexp => sexp.isInstanceOf[SchemeIf], List(substituteByChild))
    println(reduced)
  }
}
