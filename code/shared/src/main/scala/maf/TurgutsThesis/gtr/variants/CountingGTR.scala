package maf.TurgutsThesis.gtr.variants

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.core.Expression
import maf.language.scheme.SchemeExp

import scala.annotation.tailrec

object CountingGTR:
  private var OracleHits = 0
  var max = 100

  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    OracleHits = 0
    val reducedTree = BFT(tree, oracle, transformations)
    if tree.size == reducedTree.size then
      reducedTree
    else reduce(reducedTree, oracle, transformations)

  def BFT(tree: SchemeExp, oracle: SchemeExp => Boolean, transformations: List[Transformation]): SchemeExp =
    var reducedTree: SchemeExp = tree
    for (lvl <- 0 to tree.height)
      for (transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, transformation)
        if OracleHits == max then
          return reducedTree //return statement stops the BFT, because oracle hits has gotten to the maximum
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, transformation: Transformation): SchemeExp =
    for(node <- tree.levelNodes(lvl))
      for((candidateTree, candidateIdx) <- transformation.transform(tree, node).zipWithIndex)
        if candidateTree.size <= tree.size then
          if oracle(candidateTree) then
            OracleHits += 1
            transformation.hit(candidateTree, candidateIdx)
            if OracleHits == max then
              return candidateTree
            else return reduceLevelNodes(candidateTree, lvl, oracle, transformation)

    tree
    