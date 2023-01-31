package maf.TurgutsThesis.gtr.transformations.schemeLambda

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.TurgutsThesis.gtr.transformations.traits.CallReducing
import maf.TurgutsThesis.primitiveOpNames.PrimitiveOpNames
import maf.core.{Identifier, Identity}
import maf.language.scheme.*

object RemoveLambdaParamWithDeepDrop extends Transformation with CallReducing:
  override val name: String = "RemoveLambdaParamWithDeepDrop"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeLambdaParam(lambda: SchemeLambdaExp, lambdaId: Identifier): Unit = {
      for ((arg, argIdx) <- lambda.args.zipWithIndex)
        lambda.deepDropIdentifier(arg) match
          case Some(deepDroppedlambda) =>
            val lambdaReplacedTree = tree.replace(lambda, deepDroppedlambda)
            val callsReduced = reduceCallsToId(lambdaReplacedTree, lambdaId, argIdx)
            
            addTree(callsReduced)
          case _ =>
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
