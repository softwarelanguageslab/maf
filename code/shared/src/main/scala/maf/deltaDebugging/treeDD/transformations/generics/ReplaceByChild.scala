package maf.deltaDebugging.treeDD.transformations.generics

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.language.scheme.SchemeExp

object ReplaceByChild extends Transformation:
  override val name: String = "SubstituteByChild"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    val children = node.subexpressions.collect {
      case s: SchemeExp => s
    }
    
    addReplacements(children)
