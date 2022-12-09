package maf.TurgutsThesis.gtr.transformations.schemeLambda

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.*

object RemoveCallsAndReplaceByBody extends Transformation with Replacing:
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
        case Some(tree) =>
          val trees = replaceCallWithAllValues(tree, id) //this removes any non-call references to the lambda
          addTrees(trees)
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
