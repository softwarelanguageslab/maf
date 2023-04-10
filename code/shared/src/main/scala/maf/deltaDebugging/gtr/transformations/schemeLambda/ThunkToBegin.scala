package maf.deltaDebugging.gtr.transformations.schemeLambda

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.*

object ThunkToBegin extends Transformation:
  override val name: String = "RemoveCallsAndReplaceByBody"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeCallsAndReplaceByBody(lambda: SchemeLambdaExp, id: Identifier): Unit =
      val lambdaReplaced = tree.replace(lambda, SchemeBegin(lambda.body, lambda.idn))

      val callsRemoved = lambdaReplaced.map(subExp => {
        subExp match
          case SchemeFuncall(f: SchemeVarExp, args, idn) =>
            if f.id.name equals id.name then
              f
            else subExp
          case _ => subExp
      })

      addTree(callsRemoved)

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
