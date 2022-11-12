package maf.TurgutsThesis.gtr.transformations
import maf.core.{NoCodeIdentity, Identifier}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object RemoveCallsAndReplaceByBody extends Transformation:
  override val name: String = "RemoveCallsAndReplaceByBody"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeCallsAndReplaceByBody(lambda: SchemeLambdaExp, id: Identifier): Unit =
      val lambdaReplaced = tree.replace(lambda, SchemeBegin(lambda.body, lambda.idn))

      val callsRemoved = lambdaReplaced.deleteChildren({
        case SchemeFuncall(f: SchemeVarExp, _, _) =>
          f.id.name == id.name
        case _ => false
      })

      callsRemoved match
        case Some(tree) => addTree(tree)
        case _ =>

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for(lambdaBinding <- lambdaBindings)
          removeCallsAndReplaceByBody(lambdaBinding._2, lambdaBinding._1)

      case SchemeDefineVariable(name, lambda: SchemeLambdaExp, _) =>
        removeCallsAndReplaceByBody(lambda, name)

      case _ =>
  }
