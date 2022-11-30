package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifierCalls.replaceCallWithAllValues
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.language.scheme.{SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeLambdaExp, SchemeLettishExp, SchemeVarExp}

object ReplaceLambdaCalls extends Transformation with Replacing:
  override val name: String = "ReplaceLambdaCalls"
  var arr: Array[(String, Int)] = Array()

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        val lambdaBindings = lettishExp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for (lambdaBinding <- lambdaBindings)
          if arr.exists(tpl => tpl._1 equals lambdaBinding._1.name) then
            addReplacements(replaceCallWithAllValues(lettishExp, lambdaBinding._1))

      case SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
        if arr.exists(tpl => tpl._1 equals name) then
          addTrees(replaceCallWithAllValues(tree, name))

      case _ =>
