package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifier.replaceWithAllValues
import maf.TurgutsThesis.gtr.transformations.traits.{CallReducing, Replacing}
import maf.core.Identifier
import maf.language.scheme.{SchemeDefineVariable, SchemeExp, SchemeLambdaExp, SchemeLettishExp}

object RemoveLambdaParamByReplacement extends Transformation with CallReducing with Replacing:
  override val name: String = "RemoveLambdaParamByReplacement"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeLambdaParam(lambda: SchemeLambdaExp, lambdaId: Identifier): Unit = {
      for ((arg, argIdx) <- lambda.args.zipWithIndex)

        val argReplacedLambdas = replaceIdWithAllValues(lambda, arg) //replace arg (e.g. x) with all kinds of values (e.g. 1, "s", 's, ...)

        for (argReplaced <- argReplacedLambdas)
          val lambdaReplaced = tree.replace(lambda, argReplaced)
          val callsReduced = reduceCallsToId(lambdaReplaced, lambdaId, argIdx)
          addTree(callsReduced)
    }

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })
        for (lambdaBinding <- lambdaBindings)
          removeLambdaParam(lambdaBinding._2, lambdaBinding._1)

      case SchemeDefineVariable(name, lambda: SchemeLambdaExp, _) =>
        removeLambdaParam(lambda, name)

      case _ =>
  }