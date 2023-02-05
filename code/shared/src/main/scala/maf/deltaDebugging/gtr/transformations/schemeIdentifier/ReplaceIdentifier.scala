package maf.deltaDebugging.gtr.transformations.schemeIdentifier

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.{SchemeExp, SchemeVarExp}

object ReplaceIdentifier extends Transformation with Replacing:
  override val name: String = "ReplaceIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case _: SchemeVarExp =>
        if tree.size < 30 then
          addReplacements(allValues())
      case _ =>
