package maf.TurgutsThesis.gtr.transformations
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object SubstituteIdentifier extends Transformation:
  override protected val name: String = "SubstituteIdentifier"

  def replaceIdWith(exp: SchemeExp, id: Identifier, value: Value): SchemeExp =
    exp.map(subExp => {
      subExp match
        case varExp: SchemeVarExp =>
          if varExp.id.name equals id.name then
            SchemeValue(value, NoCodeIdentity)
          else varExp
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

  override def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
    var res: List[SchemeExp] = List()
    node match
      case exp: SchemeLambdaExp =>
        for(arg <- exp.args)
          replaceIdWithAllValues(exp, arg).foreach(replacement => res = res.::(replacement))
      case lettishExp: SchemeLettishExp =>
        for(id <- lettishExp.bindings.map(_._1))
          replaceIdWithAllValues(lettishExp, id).foreach(replacement => res = res.::(replacement))
      case sexp@SchemeDefineVariable(name, value, idn) =>
        val candidateTrees: List[SchemeExp] = replaceIdWithAllValues(tree, name).map(tree =>
          tree.deleteChildren(child => child == sexp)).collect({
            case Some(e) => e
          })
        return candidateTrees
      case _ =>

    res.map(newNode => tree.replace(node.path, newNode))
