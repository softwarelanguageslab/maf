package maf.TurgutsThesis.gtr.transformations
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object RemoveCalls extends Transformation:
  override protected val name: String = "RemoveCalls"
  override def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] = {
    var resTrees: List[SchemeExp] = List()

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for(lambdaBinding <- lambdaBindings)
          val lambdaId = lambdaBinding._1

          val applsRemoved: Option[SchemeExp] = tree.deleteChildren({
            case SchemeFuncall(f: SchemeVarExp, args, idn) =>
              f.id.name == lambdaId.name
            case _ => false
          })
          
          applsRemoved match
            case Some(tree) =>
              resTrees = resTrees.::(tree)
            case _ =>

      case SchemeDefineVariable(name, value, idn) =>
        value match
          case lambda: SchemeLambdaExp =>
            val applsRemoved: Option[SchemeExp] = tree.deleteChildren({
              case SchemeFuncall(f: SchemeVarExp, args, idn) =>
                f.id.name == name.name
              case _ => false
            })

            applsRemoved match
              case Some(tree) => resTrees = List(tree)
              case _ =>

          case _ =>

      case _ =>

    resTrees
  }
