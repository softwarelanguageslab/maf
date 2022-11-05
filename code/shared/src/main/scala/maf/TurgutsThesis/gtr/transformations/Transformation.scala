package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

abstract class Transformation:
  def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp]
  val name: String
  var hits: Int = 0
  def hit(tree: SchemeExp, idx: Int): Unit =
    hits += 1
    println("transformation " + name + " " + " hit the oracle!")