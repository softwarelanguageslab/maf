package maf.deltaDebugging.gtr.transformations.schemeBinding

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.*

object DefineDrop extends Transformation:
  override val name: String = "DefineDrop"

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeDefineVariable(name, value: SchemeLambdaExp, idn) =>
        None
      case definition@SchemeDefineVariable(name, value, idn) =>
        val defDropped = tree.deleteChildren(exp => exp eq definition)
        defDropped match
          case Some(defDroppedTree) =>
            val refsDropped = defDroppedTree.deleteChildren(subExp => {
              subExp match
                case varExp: SchemeVarExp =>
                  varExp.id.name equals name.name
                case _ => false
            })

            refsDropped match
              case Some(tree) =>
                addTree(tree)
              case _ =>
          case _ =>
      case _ =>
