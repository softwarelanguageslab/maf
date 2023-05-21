package maf.deltaDebugging.treeDD.transformations.schemeIdentifier

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.language.scheme.{SchemeExp, SchemeVarExp}

object ReplaceIdentifier extends Transformation:
  override val name: String = "ReplaceIdentifier"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case _: SchemeVarExp =>
        if tree.size < 100 then
          addReplacements(Replacing.allValues(node))
      case _ =>
