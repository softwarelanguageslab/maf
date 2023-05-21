package maf.deltaDebugging.treeDD.transformations

import maf.language.scheme.{SchemeExp, SchemeParser}

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

  /** transform returns all transformations */
  def transform(tree: SchemeExp, node: SchemeExp): Iterator[SchemeExp] =
    require(tree.contains(node))
    trees = List()
    replacements = List()

    transformAndAdd(tree, node) //should fill trees and replacements

    var suggestions = (trees ++ replacements.map(r => tree.replace(node, r)))
    suggestions = suggestions.map(s => SchemeParser.parse(s.prettyString()).head)

    trees = List()
    replacements = List()
    suggestions.iterator

  /** transformAndAdd is a subclass responsibility */
  protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit

  val name: String
  private var hits: Int = 0
  def getHits: Int = hits
  def hit(): Unit =
    hits += 1

  private var invocations: Int = 0
  def getInvocations: Int = invocations
  def invoke(): Unit =
    invocations += 1