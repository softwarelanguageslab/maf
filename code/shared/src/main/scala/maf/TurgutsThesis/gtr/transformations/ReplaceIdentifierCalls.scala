package maf.TurgutsThesis.gtr.transformations
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object ReplaceIdentifierCalls extends Transformation with Replacing:
  override val name: String = "ReplaceIdentifierCalls"

  def replaceCallWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    replaceWithAllValues(exp, subExp => {
      subExp match
        case SchemeFuncall(f: SchemeVarExp, _, _) =>
          f.id.name equals id.name
        case _ => false
    })

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLambdaExp =>
        for(arg <- exp.args)
          addReplacements(replaceCallWithAllValues(exp, arg))

      case lettishExp: SchemeLettishExp =>
        for(id <- lettishExp.bindings.map(_._1))
          addReplacements(replaceCallWithAllValues(lettishExp, id))

      case SchemeDefineVariable(name, _, _) =>
        addTrees(replaceCallWithAllValues(tree, name))

      case _ =>
