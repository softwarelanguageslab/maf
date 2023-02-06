package maf.deltaDebugging.gtr.transformations.generics

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.core.Identifier
import maf.language.scheme.*
import maf.language.sexp.Value

object ReplaceByValue extends Transformation with Replacing:
  override val name: String = "ReplaceByValue"

  //replace by value under certain circumstances, avoiding time/space overheads
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    if tree.size >= 30 && tree.size < 50 then
      addReplacements(allValues())
