package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.{SchemeExp, SchemeLettishExp}

object LetIdentifierDeepDrop extends Transformation:
  override val name: String = "LetIdentifierDeepDrop"

  def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        for (id <- lettishExp.bindings.map(_._1))
          lettishExp.deepDropIdentifier(id) match
            case Some(exp) => addReplacement(exp)
            case _ =>
      case _ =>
