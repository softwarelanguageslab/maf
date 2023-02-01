package maf.deltaDebugging.gtr.transformations.schemeLet

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.{SchemeExp, SchemeLettishExp}

object LetIdentifierShallowDrop extends Transformation:
  override val name: String = "LetIdentifierShallowDrop"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        for (id <- lettishExp.bindings.map(_._1))
          lettishExp.shallowDropIdentifier(id) match
            case Some(exp) => addReplacement(exp)
            case _ =>
      case _ =>
