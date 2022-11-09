package maf.TurgutsThesis.gtr.transformations
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object IfToBegin extends Transformation:
  override protected val name: String = "IfToBegin"

  override def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
    var substitutes: List[SchemeExp] = List()

    node match
      case SchemeIf(cond, cons, alt, idn) =>
        substitutes = substitutes.::(SchemeBegin(List(cond, cons), idn))
        substitutes = substitutes.::(SchemeBegin(List(cond, alt), idn))
      case _ =>

    substitutes.map(substitute => tree.replace(node.path, substitute))
