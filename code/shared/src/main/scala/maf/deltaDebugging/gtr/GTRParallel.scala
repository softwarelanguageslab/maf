package maf.deltaDebugging.gtr

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.SchemeExp
import scala.collection.parallel.CollectionConverters._

import scala.annotation.tailrec

object GTRParallel:
  @tailrec
  def reduce(tree: SchemeExp, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformations: List[Transformation]): SchemeExp =
    val reducedTree: SchemeExp = BFT(tree, oracle, onOracleHit, transformations)
    if tree.size == reducedTree.size then
      reducedTree
    else reduce(reducedTree, oracle, onOracleHit, transformations)

  private def BFT(tree: SchemeExp, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformations: List[Transformation]): SchemeExp =
    var reducedTree = tree
    for (lvl <- 0 to reducedTree.height)
      for (transformation <- transformations)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, onOracleHit, transformation)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp, lvl: Int, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformation: Transformation): SchemeExp =
    for (node <- tree.levelNodes(lvl))
      reduceNode(tree, node, oracle, onOracleHit, transformation) match
        case Some(candidate) =>
          onOracleHit(candidate)
          return reduceLevelNodes(candidate, lvl, oracle, onOracleHit, transformation)
        case _ =>
    tree

  private def reduceNode(tree: SchemeExp, node: SchemeExp, oracle: SchemeExp => Boolean, onOracleHit: SchemeExp => Unit, transformation: Transformation): Option[SchemeExp] =
    val candidates: List[SchemeExp] = transformation.transform(tree, node).filter(candidate => candidate.size < tree.size).toList
    if tree.toString.length() > 100 then
      candidates.par.find(c => {
        oracle(c)
      })
    else
      candidates.find(c => {
        oracle(c)
      })
