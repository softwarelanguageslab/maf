package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object JumpyGTR:
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for(lvl <- 0 to reducedTree.height)
      for(transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, reducedTree.levelNodes(lvl), oracle, transformation)
        if !(reducedTree == tree) then
          return reduce(reducedTree, oracle, transformations)
    //println("QuickGTR total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
    tree

  private def reduceLevelNodes(tree: SchemeExp, lvlNodes: List[SchemeExp], oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    for(node <- lvlNodes)
      for((candidateTree, candidateIdx) <- transformation.transform(tree, node).zipWithIndex)
        if candidateTree.size < tree.size then
          if oracle(candidateTree) then
            transformation.hit(candidateTree, candidateIdx)
            return candidateTree
    tree
