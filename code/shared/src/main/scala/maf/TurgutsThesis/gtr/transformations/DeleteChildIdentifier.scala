package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.DeleteChildSimple.deleteChildLettishExp
import maf.core.{Identifier, Identity}
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambda, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp}

object DeleteChildIdentifier extends Transformation:
  override val name: String = "DeleteChildIdentifier"

  def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
    var res: List[SchemeExp] = List()
    node match
      case s: SchemeLettishExp =>
        res = deleteChildLettishExp(s)
      case _ =>

    res.map(nodeSubstitute => {
      tree.replace(node.path, nodeSubstitute)
    })

  def deleteChildLettishExp(lettishExp: SchemeLettishExp): List[SchemeExp] =
    var res: List[SchemeExp] = List()
    val bindings = lettishExp.bindings

    for (i <- bindings.indices)
      val id = bindings(i)._1
      val referencesShallowDropped = lettishExp.shallowDropIdentifier(id)
      val referencesDeepDropped = lettishExp.deepDropIdentifier(id)

      res = res.::(referencesDeepDropped)
      res = res.::(referencesShallowDropped)

    res



