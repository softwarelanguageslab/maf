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

          val lambdaReplaced = tree.replace(lambda, SchemeBegin(lambda.body, lambda.idn))

          val callsRemoved = lambdaReplaced.deleteChildren({
            case SchemeFuncall(f: SchemeVarExp, _, _) =>
              f.id.name == lambdaId.name
            case _ => false
          })

          callsRemoved match
            case Some(tree) => addTree(tree)
            case _ =>

      case SchemeDefineVariable(name, lambda: SchemeLambdaExp, _) =>
        val lambdaReplaced = tree.replace(lambda, SchemeBegin(lambda.body, lambda.idn))

        val callsRemoved = lambdaReplaced.deleteChildren({
          case SchemeFuncall(f: SchemeVarExp, _, _) =>
            f.id.name == name.name
          case _ => false
        })

        callsRemoved match
          case Some(tree) => addTree(tree)
          case _ =>

      case _ =>
  }
