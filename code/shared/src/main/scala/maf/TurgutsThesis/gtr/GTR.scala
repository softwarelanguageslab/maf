package maf.TurgutsThesis.gtr

import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[SchemeExp => List[SchemeExp]]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for(lvl <- 0 to reducedTree.height)
      for(template <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, reducedTree.levelNodes(lvl), oracle, template)
    if tree.size == reducedTree.size then
      reducedTree
    else reduce(reducedTree, oracle, transformations)

  private def reduceLevelNodes(tree: SchemeExp, lvlNodes: List[SchemeExp], oracle: SchemeExp => Boolean, transformation: SchemeExp => List[SchemeExp]): SchemeExp =
    var reducedTree: SchemeExp = tree
    var conf: Map[SchemeExp, SchemeExp] = Map()
    for(node <- lvlNodes)
      conf = conf + (node -> node)

    @tailrec
    def improve(): Unit =
      var improvementFound = false
      for(node <- lvlNodes)
        var currentReplacement = conf(node)
        for(candidateReplacement <- transformation(node))
          if candidateReplacement.size < currentReplacement.size then
            val newTree = reducedTree.replace(currentReplacement, candidateReplacement)
            if oracle(newTree) then
              improvementFound = true
              reducedTree = newTree
              currentReplacement = candidateReplacement
              conf = conf + (node -> candidateReplacement)
      if improvementFound then improve()

    improve()
    reducedTree
