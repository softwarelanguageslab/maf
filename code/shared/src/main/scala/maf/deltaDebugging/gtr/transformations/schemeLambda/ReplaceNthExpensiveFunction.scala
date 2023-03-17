package maf.deltaDebugging.gtr.transformations.schemeLambda

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.deltaDebugging.primitiveOpNames.PrimitiveOpNames
import maf.core.Identity
import maf.language.scheme.*

class ReplaceNthExpensiveFunction(arr: Array[(String, Int)], n: Int) extends Transformation:
  override val name: String = "ReplaceNthExpensiveFunction"
  private val nthExpensiveName: String = arr(n)._1

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        val lambdaBindings = lettishExp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for (lambdaBinding <- lambdaBindings)
          if lambdaBinding._1.name equals nthExpensiveName then
            val newLet = lettishExp.dropBinding(lambdaBinding._1.name)
            val newTree = tree.replace(lettishExp, newLet)
            addTrees(Replacing.replaceCallWithAllValues(newTree, lambdaBinding._1))

      case defExp@SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
        if name.name equals nthExpensiveName then
          val newTree = tree.deleteChildren(child => child eq defExp) //removes the definition
          newTree match
            case Some(newTree) =>
              addTrees(Replacing.replaceCallWithAllValues(newTree, name))
            case _ =>

      case _ =>