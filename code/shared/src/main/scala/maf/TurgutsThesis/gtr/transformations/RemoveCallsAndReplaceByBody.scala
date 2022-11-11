package maf.TurgutsThesis.gtr.transformations
import maf.core.NoCodeIdentity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object RemoveCallsAndReplaceByBody extends Transformation:
  override val name: String = "RemoveCallsAndReplaceByBody"
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for(lambdaBinding <- lambdaBindings)
          val lambda = lambdaBinding._2
          val lambdaId = lambdaBinding._1

          val applsRemoved: Option[SchemeExp] = tree.deleteChildren({
            case SchemeFuncall(f: SchemeVarExp, _, _) =>
              f.id.name == lambdaId.name
            case _ => false
          })

          applsRemoved match
            case Some(tree) =>
              val candidate = tree.map(sexp => {
                if sexp eql lambda then
                  SchemeBegin(lambda.body, NoCodeIdentity)
                else sexp
              })
              addTree(candidate)
            case _ =>

      case SchemeDefineVariable(name, value, _) =>
        value match
          case lambda: SchemeLambdaExp =>
            val applsRemoved: Option[SchemeExp] = tree.deleteChildren({
              case SchemeFuncall(f: SchemeVarExp, _, _) =>
                f.id.name == name.name
              case _ => false
            })

            applsRemoved match
              case Some(tree) =>
                val candidate = tree.map(sexp => {
                  if sexp eql lambda then
                    SchemeBegin(lambda.body, NoCodeIdentity)
                  else sexp
                })
                addTree(candidate)
              case _ =>

          case _ =>

      case _ =>
  }
