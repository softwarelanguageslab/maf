package maf.TurgutsThesis.gtr

import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, templates: List[SchemeExp => List[SchemeExp]]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for(lvl <- 0 to reducedTree.height)
      for(template <- templates)
        reducedTree = reduceLevelNodes(reducedTree, reducedTree.levelNodes(lvl), oracle, template)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvlNodes: List[SchemeExp], oracle: SchemeExp => Boolean, template: SchemeExp => List[SchemeExp]): SchemeExp =
    var reducedTree: SchemeExp = tree
    var conf: Map[SchemeExp, SchemeExp] = Map()
    for(node <- lvlNodes)
      conf = conf + (node -> node)

    @tailrec
    def improve(): Unit =
      var improvementFound = false
      for(node <- lvlNodes)
        var currentReplacement = conf(node)
        for(candidateReplacement <- template(node))
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
