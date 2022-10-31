package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

def substituteByChild(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
  val children = node.subexpressions.collect {
    case s: SchemeExp => s
  }

  children.map(child => tree.replace(node, child))
