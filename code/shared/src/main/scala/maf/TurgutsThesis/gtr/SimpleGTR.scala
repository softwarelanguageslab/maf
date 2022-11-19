package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object SimpleGTR:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    val reducedTree: SchemeExp = BFT(tree, oracle, transformations)
    if tree.size == reducedTree.size then
      //println("GTR total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
      reducedTree
    else reduce(reducedTree, oracle, transformations)

  private def BFT(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree = tree
    for (lvl <- 0 to reducedTree.height)
      for (transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, transformation, 0)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, transformation: Transformation, dropIdx: Int): SchemeExp =
    for((node, nodeIdx) <- tree.levelNodes(lvl).drop(dropIdx).zipWithIndex)
      for((candidateTree, candidateIdx) <- transformation.transform(tree, node).zipWithIndex)
        if candidateTree.size < tree.size then
          if oracle(candidateTree) then
            transformation.hit(candidateTree, candidateIdx)
            return reduceLevelNodes(candidateTree, lvl, oracle, transformation, nodeIdx)

    tree
