package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.DeleteChildSimple.deleteChildLettishExp
import maf.core.{Identifier, Identity}
import maf.language.scheme.{SchemeBegin, SchemeExp, SchemeFuncall, SchemeLambda, SchemeLet, SchemeLetStar, SchemeLetrec, SchemeLettishExp}

object DeleteChildIdentifier extends Transformation:
  override val name: String = "DeleteChildIdentifier"

  def transform(tree: SchemeExp, node: SchemeExp): List[SchemeExp] =
    var res: List[SchemeExp] = List()
    node match
      case s: SchemeLet =>
        res = deleteChildLettishExp(s, SchemeLet.apply)
      case s: SchemeLetStar =>
        res = deleteChildLettishExp(s, SchemeLetStar.apply)
      case s: SchemeLetrec =>
        res = deleteChildLettishExp(s, SchemeLetrec.apply)
      case _ =>

    res.map(nodeSubstitute => {
      tree.replace(node, nodeSubstitute)
    })

  def deleteChildLettishExp(lettishExp: SchemeLettishExp,
                            factoryMethod: (List[(Identifier, SchemeExp)], List[SchemeExp], Identity) => SchemeLettishExp): List[SchemeExp] =
    var res: List[SchemeExp] = List()
    val bindings = lettishExp.bindings

    for (i <- bindings.indices)
      val id = bindings(i)._1
      val referencesShallowDropped = lettishExp.shallowDropIdentifier(id)
      val referencesDeepDropped = lettishExp.deepDropIdentifier(id)

      res = res.::(referencesDeepDropped)
      res = res.::(referencesShallowDropped)

    res



