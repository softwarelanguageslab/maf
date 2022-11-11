package maf.TurgutsThesis.gtr.transformations
import maf.core.Identifier
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object ReplaceIdentifier extends Transformation with Replacing:
  override val name: String = "ReplaceIdentifier"

  def replaceIdWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    replaceWithAllValues(exp, subExp => {
      subExp match
        case varExp: SchemeVarExp =>
          varExp.id.name equals id.name
        case _ => false
    })

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLambdaExp =>
        for(arg <- exp.args)
          addReplacements(replaceIdWithAllValues(exp, arg))
      case lettishExp: SchemeLettishExp =>
        for(id <- lettishExp.bindings.map(_._1))
          addReplacements(replaceIdWithAllValues(lettishExp, id))
      case SchemeDefineVariable(name, _, _) =>
        addTrees(replaceIdWithAllValues(tree, name))
      case _ =>
