/*
package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.{SchemeExp, SchemeLambdaExp, SchemeLettishExp}

object ShallowDropIdentifier extends Transformation:
  override val name: String = "ShallowDropIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        val bindings = lettishExp.bindings

        for (i <- bindings.indices)
          val id = bindings(i)._1
          val referencesShallowDropped = lettishExp.shallowDropIdentifier(id)
          addReplacement(referencesShallowDropped)
          /*
      case lambda: SchemeLambdaExp =>
        for(a <- lambda.args)
          val referencesShallowDropped = lambda.shallowDropIdentifier(id)
          addReplacement(referencesShallowDropped)
          */
      case _ =>

 */