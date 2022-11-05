package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

abstract class Transformation:
  def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp]
  protected val name: String
  private var hits: Int = 0
  def hit(tree: SchemeExp, idx: Int): Unit =
    hits += 1
    println("transformation " + name + " " + " hits oracle w idx: " + idx)