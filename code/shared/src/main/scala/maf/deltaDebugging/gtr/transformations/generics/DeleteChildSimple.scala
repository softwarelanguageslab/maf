package maf.deltaDebugging.gtr.transformations.generics

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.core.{Identifier, Identity}
import maf.language.scheme.*

object DeleteChildSimple extends Transformation:
  override val name: String = "DeleteChildSimple"
  protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeLambda(name, args, body, annotation, idn) =>
        if body.length > 1 then
          for (i <- body.indices)
            addReplacement(SchemeLambda(name, args, body.take(i) ++ body.drop(i + 1), annotation, idn))
      case SchemeBegin(exps, idn) =>
        if exps.length > 1 then
          for (i <- exps.indices)
            addReplacement(SchemeBegin(exps.take(i) ++ exps.drop(i + 1), idn))
      case lettishExp: SchemeLettishExp =>
        val bindings = lettishExp.bindings
        val body = lettishExp.body

        if body.length > 1 then
          for (i <- body.indices)
            addReplacement(lettishExp.dropBodyExp(i))

        for ((identifier, exp) <- bindings)
          addReplacement(lettishExp.dropBinding(identifier.name))
      case _ =>