package maf.TurgutsThesis.gtr.transformations
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object ReplaceIdentifierCalls extends Transformation with Replacing:
  override protected val name: String = "SubstituteIdentifierCalls"

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
        val lambdaBindings = lettishExp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })
        for(id <- lambdaBindings.map(_._1))
          addReplacements(replaceCallWithAllValues(lettishExp, id))

      case SchemeDefineVariable(name, _, _) =>
        tree.deleteChildren(child => child == node) match
          case Some(tree) =>
            addTrees(replaceCallWithAllValues(tree, name))
          case _ =>
      case _ =>
