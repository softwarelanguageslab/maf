package maf.TurgutsThesis.gtr.transformations.generics

import maf.TurgutsThesis.gtr.transformations.Transformation
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.core.Identifier
import maf.language.scheme.*
import maf.language.sexp.Value

object ReplaceByValue extends Transformation with Replacing:
  override val name: String = "ReplaceIdentifier"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    if node.size > 15 then //only apply this to large nodes, otherwise its too expensive
      addReplacements(allValues())
