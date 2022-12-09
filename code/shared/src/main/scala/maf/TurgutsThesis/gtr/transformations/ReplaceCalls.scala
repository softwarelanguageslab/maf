package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.core.Identifier
import maf.language.scheme.{SchemeExp, SchemeFuncall, SchemeVarExp, SchemeLettishExp, SchemeDefineVariable, SchemeLambdaExp}

object ReplaceCalls extends Transformation with Replacing:
  override val name: String = "ReplaceCalls"
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {

  node match
    case exp: SchemeLettishExp =>
      val lambdaBindings = exp.bindings.collect({
        case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
      })

      for(lambdaBinding <- lambdaBindings)
        val newTree = tree.replace(exp, exp.dropBinding(lambdaBinding._1.name))
        addTrees(replaceCallWithAllValues(newTree, lambdaBinding._1))

    case defExp@SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
      val newTree = tree.deleteChildren(child => child eq defExp) //removes the definition
      newTree match
        case Some(newTree) =>
          addTrees(replaceCallWithAllValues(newTree, name))
        case _ =>

    case _ =>
  }

