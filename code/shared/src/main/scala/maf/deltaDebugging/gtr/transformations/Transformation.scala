package maf.deltaDebugging.gtr.transformations

import maf.language.scheme.SchemeExp

abstract class Transformation:
  var trees: List[SchemeExp] = List()
  var replacements: List[SchemeExp] = List()

  def addTree(t: SchemeExp): Unit =
    trees = trees.::(t)
    
  def addTrees(ts: List[SchemeExp]): Unit =
    ts.foreach(addTree)

  def addReplacement(replacement: SchemeExp): Unit =
    replacements = replacements.::(replacement)
    
  def addReplacements(rs: List[SchemeExp]): Unit =
    rs.foreach(addReplacement)

  /** transformIdx returns the nth transformation */
  def transformIdx(tree: SchemeExp, node: SchemeExp, idx: Int): Option[SchemeExp] =
    require(tree.contains(node))
    trees = List()
    replacements = List()

    transformAndAdd(tree, node) //should fill trees and replacements

    val transformations = (trees ++ replacements.map(r => tree.replace(node, r)))
                          .filterNot(newTree => tree eql newTree) //if a transformation suggest a tree that is eql to the current tree, discard that suggestions

    try Some(transformations(idx))
    catch case _: Throwable => None

  /** transform returns all transformations */
  def transform(tree: SchemeExp, node: SchemeExp): Iterator[SchemeExp] =
    require(tree.contains(node))
    trees = List()
    replacements = List()

    transformAndAdd(tree, node) //should fill trees and replacements

    val iter = (trees ++ replacements.map(r => tree.replace(node, r)))
      .filterNot(newTree => tree eql newTree).iterator //if a transformation suggest a tree that is eql to the current tree, discard that suggestions

    trees = List()
    replacements = List()
    iter

  /** transformAndAdd is a subclass responsibility */
  protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit

  val name: String
  private var hits: Int = 0
  def getHits: Int = hits
  def hit(tree: SchemeExp, idx: Int): Unit =
    hits += 1