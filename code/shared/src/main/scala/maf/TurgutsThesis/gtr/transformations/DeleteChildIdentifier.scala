package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.DeleteChildSimple.deleteChildLettishExp
import maf.core.{Identifier, Identity}
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambda, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp}

object DeleteChildIdentifier extends Transformation:
  override val name: String = "DeleteChildIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case s: SchemeLettishExp =>
        deleteChildLettishExp(s)
      case _ =>

  def deleteChildLettishExp(lettishExp: SchemeLettishExp): Unit =
    val bindings = lettishExp.bindings

    for (i <- bindings.indices)
      val id = bindings(i)._1
      lettishExp.shallowDropIdentifier(id) match
        case Some(exp) => addReplacement(exp)
        case _ =>
      val referencesDeepDropped = lettishExp.deepDropIdentifier(id)

      addReplacement(referencesDeepDropped)
