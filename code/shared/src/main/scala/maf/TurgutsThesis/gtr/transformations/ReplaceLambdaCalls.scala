package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeLambdaExp, SchemeVarExp}

object ReplaceLambdaCalls extends Transformation with Replacing:
  override val name: String = "ReplaceLambdaCalls"
  var arr: Array[(String, Int)] = Array()

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLambdaExp =>
        exp.name match
          case Some(lambdaName) =>
            if arr.exists(tpl => tpl._1 equals lambdaName) then
              val lambdaDeleted: Option[SchemeExp] = tree.deleteChildren(subExp => {
                subExp eq exp
              })

              lambdaDeleted match
                case Some(tree) =>
                  val suggestions = replaceWithAllValues(tree, subExp => {
                    subExp match
                      case SchemeFuncall(f: SchemeVarExp, _, _) =>
                        f.id.name equals lambdaName
                      case varExp: SchemeVarExp => varExp.id.name equals lambdaName
                      case _ => false
                  })

                  addTrees(suggestions)
                case _ =>
          case _ =>
      case _ =>
