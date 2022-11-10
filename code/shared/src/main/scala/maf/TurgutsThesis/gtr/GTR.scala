package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    //call setPaths() first on the tree before we really start reducing
    reduceLoop(tree, oracle, transformations)

  @tailrec
  private def reduceLoop(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for(lvl <- 0 to reducedTree.height)
      for(transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, transformation)
    if tree.size == reducedTree.size then
      println("GTR total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
      reducedTree
    else reduceLoop(reducedTree, oracle, transformations)

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      for((candidateTree, candidateIdx) <- transformation.transform(tree, node).zipWithIndex)
        if candidateTree.size < tree.size then
          if oracle(candidateTree) then
            transformation.hit(candidateTree, candidateIdx)
            return reduceLevelNodes(candidateTree, lvl, oracle, transformation)

    tree
