package maf.TurgutsThesis.gtr.transformations.schemeIdentifier

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.language.scheme.{SchemeExp, SchemeVarExp}

object ReplaceIdentifier extends Transformation with Replacing:
  override val name: String = "ReplaceIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case s: SchemeVarExp =>
        addReplacements(allValues())
      case _ =>
