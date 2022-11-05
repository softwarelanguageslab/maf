package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for(lvl <- 0 to reducedTree.height)
      for(transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, reducedTree.levelNodes(lvl), oracle, transformation)
    if tree.size == reducedTree.size then
      println("total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
      reducedTree
    else reduce(reducedTree, oracle, transformations)

  private def reduceLevelNodes(tree: SchemeExp, lvlNodes: List[SchemeExp], oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    var reducedTree: SchemeExp = tree
    
    @tailrec
    def improve(): Unit =
      var improvementFound = false
      for(node <- lvlNodes)
        for((candidateTree, candidateIdx) <- transformation.transform(reducedTree, node).zipWithIndex)
          if candidateTree.size < reducedTree.size then
            if oracle(candidateTree) then
              transformation.hit(candidateTree, candidateIdx)
              improvementFound = true
              reducedTree = candidateTree
      if improvementFound then improve()

    improve()
    reducedTree
