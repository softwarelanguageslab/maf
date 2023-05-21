package maf.deltaDebugging.treeDD.variants

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.core.Expression
import maf.language.scheme.SchemeExp

import scala.annotation.tailrec

object GTR:
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
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, transformation)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      for(candidateTree <- transformation.transform(tree, node))
        if candidateTree.size <= tree.size then
          if oracle(candidateTree) then
            return reduceLevelNodes(candidateTree, lvl, oracle, transformation)

    tree
