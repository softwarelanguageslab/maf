package maf.deltaDebugging.treeDD

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.language.scheme.SchemeExp
import scala.collection.parallel.CollectionConverters._

import scala.annotation.tailrec

import scala.annotation.tailrec

object ParallelSchemeReduce:
  var fixpoint = true
  @tailrec
  def reduce(tree: SchemeExp,
             oracle: SchemeExp => Boolean,
             onOracleHit: SchemeExp => Unit,
             transformations: List[Transformation]): SchemeExp =
    fixpoint = true
    val reducedTree: SchemeExp = BFT(tree, oracle, onOracleHit, transformations)
    if fixpoint then
      reducedTree
    else reduce(reducedTree, oracle, onOracleHit, transformations)

  private def BFT(tree: SchemeExp,
                  oracle: SchemeExp => Boolean,
                  onOracleHit: SchemeExp => Unit,
                  transformations: List[Transformation]): SchemeExp =
    var reducedTree = tree
    for (lvl <- 0 to reducedTree.height)
        reducedTree = reduceLevelNodes(reducedTree, lvl, oracle, onOracleHit, transformations)
    reducedTree

  private def reduceLevelNodes(tree: SchemeExp,
                               lvl: Int,
                               oracle: SchemeExp => Boolean,
                               onOracleHit: SchemeExp => Unit,
                               transformations: List[Transformation]): SchemeExp =
    val nodes = tree.levelNodes(lvl)
    var candidates: List[SchemeExp] = List()
    for (t <- transformations) {
      for(node <- nodes) {
        for(candidate <- t.transform(tree, node)) {
          candidates = candidates.::(candidate)
          if candidates.size == 16 then
            candidates.par.find(oracle(_)) match
              case Some(c) =>
                fixpoint = false
                onOracleHit(c)
                return reduceLevelNodes(c, lvl, oracle, onOracleHit, transformations)
              case _ =>
                candidates = List()
        }
      }
    }

    candidates.find(oracle(_)) match
      case Some(c) =>
        fixpoint = false
        onOracleHit(c)
        reduceLevelNodes(c, lvl, oracle, onOracleHit, transformations)
      case _ => tree
