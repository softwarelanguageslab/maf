package maf.deltaDebugging.gtr.transformations.generics

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.SchemeExp

object ReplaceBySimpleValue extends Transformation with Replacing:
  override val name: String = "ReplaceBySimpleValue"

  //replace by value under certain circumstances, avoiding time/space overheads
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    if tree.size < 10 then
      addReplacements(values)
