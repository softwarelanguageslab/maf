package maf.deltaDebugging.gtr

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp

object GTRLayered:
  def reduce(tree: SchemeExp,
             oracle: SchemeExp => Boolean,
             onOracleHit: SchemeExp => Unit,
             transformations: List[Transformation],
             deadCodeRemover: Option[SchemeExp => Option[SchemeExp]] = None): SchemeExp =
    var reduced = tree
    for(i <- 1 to transformations.length)
      val subset = transformations.take(i)
      reduced = GTR.reduce(reduced, oracle, onOracleHit, subset, deadCodeRemover)
    reduced

