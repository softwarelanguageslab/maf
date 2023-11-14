package maf.deltaDebugging.treeDD

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.language.scheme.SchemeExp

object LayeredSchemeReduce:
  def reduce(tree: SchemeExp,
             oracle: SchemeExp => Boolean,
             onOracleHit: SchemeExp => Unit,
             transformations: List[Transformation],
             deadCodeRemover: Option[SchemeExp => Option[SchemeExp]] = None,
             layerSize: Int
            ): SchemeExp =
    var reduced = tree
    var subset: List[Transformation] = List()
    while subset.size != transformations.size do
      subset = transformations.take(subset.size + layerSize)
      reduced = SchemeReduce.reduce(reduced, oracle, onOracleHit, subset, deadCodeRemover)
    reduced

