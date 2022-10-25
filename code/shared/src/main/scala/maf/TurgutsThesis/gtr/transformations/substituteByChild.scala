package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

def substituteByChild(node: SchemeExp): List[SchemeExp] =
  node.subexpressions.collect {
    case s: SchemeExp => s
  }
