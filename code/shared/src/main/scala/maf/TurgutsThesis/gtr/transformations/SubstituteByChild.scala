package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

object SubstituteByChild extends Transformation:
  override val name: String = "SubstituteByChild"

  override def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
    val children = node.subexpressions.collect {
      case s: SchemeExp => s
    }
    children.map(child => tree.replace(node, child))
