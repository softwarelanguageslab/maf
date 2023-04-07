package maf.deltaDebugging.gtr.transformations.schemeSequencify

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.*

object IfToBegin extends Transformation:
  override val name: String = "IfToBegin"

  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeIf(cond, cons, alt, idn) =>
        addReplacement(SchemeBegin(List(cond, cons), idn))
        addReplacement(SchemeBegin(List(cond, alt), idn))
      case _ =>
