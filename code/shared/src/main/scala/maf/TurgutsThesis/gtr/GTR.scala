package maf.TurgutsThesis.gtr

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import maf.core.Expression

import scala.annotation.tailrec

object GTR:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformations: List[Transformation]): SchemeExp =
    val reducedTree: SchemeExp = BFT(tree, oracle, onOracleHit, transformations)
    if tree.size == reducedTree.size then
      //println("GTR total transformation count: " + transformations.map(_.getHits).fold(0)(_ + _))
      reducedTree
    else reduce(reducedTree, oracle, onOracleHit, transformations)
    
  private def BFT(tree: SchemeExp, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformations: List[Transformation]): SchemeExp =
    var reducedTree = tree
    for (lvl <- 0 to reducedTree.height)
      for (transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, onOracleHit, transformation)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformation: Transformation): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      var idx = 0
      var candidateTree: Option[SchemeExp] = None

      while
        candidateTree = transformation.transformIdx(tree, node, idx)
        candidateTree.nonEmpty
      do
        idx += 1
        if candidateTree.get.size <= tree.size then
          if oracle(candidateTree.get) then
            onOracleHit(candidateTree.get)
            transformation.hit(candidateTree.get, idx)
            return reduceLevelNodes(candidateTree.get, lvl, oracle, onOracleHit, transformation)
    tree
