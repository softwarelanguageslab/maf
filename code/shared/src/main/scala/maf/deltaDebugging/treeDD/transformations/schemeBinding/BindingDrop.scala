package maf.deltaDebugging.treeDD.transformations.schemeBinding

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.language.scheme.{SchemeExp, SchemeLettishExp}

object BindingDrop extends Transformation:
  override val name: String = "BindingDrop"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        for (id <- lettishExp.bindings.map(_._1))
          lettishExp.deepDropIdentifier(id) match
            case Some(exp) => addReplacement(exp)
            case _ =>
      case _ =>
