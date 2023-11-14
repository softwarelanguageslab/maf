package maf.deltaDebugging.treeDD.transformations.generics

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.core.Identifier
import maf.language.scheme.*
import maf.language.sexp.Value

object ReplaceByValue extends Transformation:
  override val name: String = "ReplaceByValue"

  //replace by value under certain circumstances, avoiding time/space overheads
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    if tree.size < 40 then
      addReplacements(Replacing.allValues(node))
