package maf.TurgutsThesis.gtr.transformations

import maf.language.scheme.SchemeExp

object SubstituteByChild extends Transformation:
  override val name: String = "SubstituteByChild"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    val children = node.subexpressions.collect {
      case s: SchemeExp => s
    }
    
    addReplacements(children)
