package maf.TurgutsThesis.gtr.transformations
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object SubstituteIdentifierCalls extends Transformation:
  override protected val name: String = "SubstituteIdentifierCalls"

  def replaceIdWith(exp: SchemeExp, id: Identifier, value: Value): SchemeExp =
    exp.map(subExp => {
      subExp match
        case fcall@SchemeFuncall(f: SchemeVarExp, _, _) =>
          if f.id.name equals id.name then
            SchemeValue(value, NoCodeIdentity)
          else fcall
        case any => any
    })

  def replaceIdWithAllValues(exp: SchemeExp, id: Identifier): List[SchemeExp] =
    List(
      replaceIdWith(exp, id, Value.Integer(1)),
      replaceIdWith(exp, id, Value.String("S")),
      replaceIdWith(exp, id, Value.Boolean(true)),
      replaceIdWith(exp, id, Value.Boolean(false)),
      replaceIdWith(exp, id, Value.Symbol("S")),
    )

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLambdaExp =>
        for(arg <- exp.args)
          addReplacements(replaceIdWithAllValues(exp, arg))
      case lettishExp: SchemeLettishExp =>
        for(id <- lettishExp.bindings.map(_._1))
          addReplacements(replaceIdWithAllValues(lettishExp, id))
      case sexp@SchemeDefineVariable(name, value, idn) =>
        val candidateTrees: List[SchemeExp] = replaceIdWithAllValues(tree, name).map(tree =>
          tree.deleteChildren(child => child == sexp)).collect({
          case Some(e) => e
        })
        addTrees(candidateTrees)
      case _ =>
