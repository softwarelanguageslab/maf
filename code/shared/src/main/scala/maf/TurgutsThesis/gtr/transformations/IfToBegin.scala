package maf.TurgutsThesis.gtr.transformations
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object IfToBegin extends Transformation:
  override val name: String = "IfToBegin"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeIf(cond, cons, alt, idn) =>
        addReplacement(SchemeBegin(List(cond, cons), idn))
        addReplacement(SchemeBegin(List(cond, alt), idn))
      case _ =>
