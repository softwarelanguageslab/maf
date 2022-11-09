package maf.TurgutsThesis.gtr.transformations
import maf.core.{Identifier, Identity}
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarArgLambda, SchemeVarExp, SymbolicHole, SymbolicVar}

object RemoveLambdaParam extends Transformation:
  override protected val name: String = "removeLambdaParam"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeLambdaParam(lambda: SchemeLambdaExp, id: Identifier): Unit = {
      for ((arg, argIdx) <- lambda.args.zipWithIndex)
        val reducedLambda = lambda match
          case SchemeLambda(name, args, body, annotation, idn) =>
            SchemeLambda(name, args.take(argIdx) ++ args.drop(argIdx + 1), body, annotation, idn)
          case SchemeVarArgLambda(name, args, vararg, body, annotation, idn) =>
            SchemeVarArgLambda(name, args.take(argIdx) ++ args.drop(argIdx + 1), vararg, body, annotation, idn)

        val lambdaReplacedTree = tree.replace(lambda.path, reducedLambda)
        val callsReducedTree = lambdaReplacedTree.map(sexp => {
          sexp match
            case SchemeFuncall(f: SchemeVarExp, fArgs, idn) =>
              if f.id.name == id.name then
                SchemeFuncall(f, fArgs.take(argIdx) ++ fArgs.drop(argIdx + 1), idn)
              else sexp
            case _ => sexp
        })

        addTree(callsReducedTree)
    }

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        if lambdaBindings.length < 3 then //otherwise, this gets very expensive
          for (lambdaBinding <- lambdaBindings)
            val lambdaId = lambdaBinding._1
            val lambdaExp = lambdaBinding._2
            removeLambdaParam(lambdaExp, lambdaId)

      case SchemeDefineVariable(name, lambda: SchemeLambdaExp, _) =>
        removeLambdaParam(lambda, name)
      case _ =>

  }
