package maf.deltaDebugging.treeDD.variants

import maf.deltaDebugging.treeDD.SchemeReduce
import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.core.Expression
import maf.language.scheme.SchemeExp

import scala.annotation.tailrec

object FirstInternalGTR:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree: SchemeExp = tree
    if reducedTree.height < 3 then
      SchemeReduce.reduce(reducedTree, oracle, identity, transformations)
    else
      for(lvl <- 0 to (reducedTree.height - 3))
        for(transformation <- transformations)
          reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, transformation)
      if tree.size == reducedTree.size then
        SchemeReduce.reduce(reducedTree, oracle, identity, transformations)
      else reduce(reducedTree, oracle, transformations)

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      for((candidateTree, candidateIdx) <- transformation.transform(tree, node).zipWithIndex)
        if candidateTree.size <= tree.size then
          if oracle(candidateTree) then
            return reduceLevelNodes(candidateTree, lvl, oracle, transformation)

    tree
