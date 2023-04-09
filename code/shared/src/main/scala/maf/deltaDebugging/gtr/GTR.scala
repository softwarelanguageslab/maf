package maf.deltaDebugging.gtr

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  @tailrec
  def reduce(tree: SchemeExp,
             oracle: SchemeExp => Boolean,
             onOracleHit: SchemeExp => Unit,
             transformations: List[Transformation],
             deadCodeRemover: Option[SchemeExp => Option[SchemeExp]] = None): SchemeExp =
    val reducedTree: SchemeExp = BFT(tree, oracle, onOracleHit, transformations, deadCodeRemover)
    if tree.size == reducedTree.size then
      //println("GTR total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
      reducedTree
    else reduce(reducedTree, oracle, onOracleHit, transformations, deadCodeRemover)
    
  private def BFT(tree: SchemeExp,
                  oracle: SchemeExp => Boolean,
                  onOracleHit: SchemeExp => Unit,
                  transformations: List[Transformation],
                  deadCodeRemover: Option[SchemeExp => Option[SchemeExp]]): SchemeExp =
    var reducedTree = tree
    for (lvl <- 0 to reducedTree.height)
      for (transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, onOracleHit, transformation, deadCodeRemover)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp,
                               lvl: Int,
                               oracle: SchemeExp => Boolean,
                               onOracleHit: SchemeExp => Unit,
                               transformation: Transformation,
                               deadCodeRemover: Option[SchemeExp => Option[SchemeExp]]): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      for (candidateTree <- transformation.transform(tree, node))
        if candidateTree.size <= tree.size then
          transformation.invoke()
          if oracle(candidateTree) then
            onOracleHit(candidateTree)
            transformation.hit()
            deadCodeRemover match
              case Some(remover) =>
                val maybeRemoved = remover(candidateTree)
                maybeRemoved match
                  case Some(removed) =>
                    if oracle(removed) then
                      println("GTR pre:  " + candidateTree.size)
                      println("GTR post: " + removed.size)
                      return reduceLevelNodes(removed, lvl, oracle, onOracleHit, transformation, deadCodeRemover)
                    else return reduceLevelNodes(candidateTree, lvl, oracle, onOracleHit, transformation, deadCodeRemover)
                  case _ => return reduceLevelNodes(candidateTree, lvl, oracle, onOracleHit, transformation, deadCodeRemover)
              case _ => return reduceLevelNodes(candidateTree, lvl, oracle, onOracleHit, transformation, deadCodeRemover)
    tree
