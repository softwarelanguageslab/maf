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
      for(template <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, reducedTree.levelNodes(lvl), oracle, template)
    if tree.size == reducedTree.size then
      reducedTree
    else reduce(reducedTree, oracle, transformations)

  private def reduceLevelNodes(tree: SchemeExp, lvlNodes: List[SchemeExp], oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    var reducedTree: SchemeExp = tree
    
    @tailrec
    def improve(): Unit =
      var improvementFound = false
      for(node <- lvlNodes)
        for(candidateTree <- transformation.transform(reducedTree, node))
          if candidateTree.size < reducedTree.size then
            if oracle(candidateTree) then
              improvementFound = true
              reducedTree = candidateTree
      if improvementFound then improve()

    improve()
    reducedTree
