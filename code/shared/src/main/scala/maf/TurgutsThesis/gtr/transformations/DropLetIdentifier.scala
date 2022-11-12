package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.DeleteChildSimple.deleteChildLettishExp
import maf.core.{Identifier, Identity}
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambda, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp}

object DropLetIdentifier extends Transformation:
  override val name: String = "DropLetIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        for (id <- lettishExp.bindings.map(_._1))
          lettishExp.shallowDropIdentifier(id) match
            case Some(exp) => addReplacement(exp)
            case _ =>
          lettishExp.deepDropIdentifier(id) match
            case Some(exp) => addReplacement(exp)
            case _ =>
      case _ =>
