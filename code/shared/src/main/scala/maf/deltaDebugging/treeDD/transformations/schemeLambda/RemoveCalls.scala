package maf.deltaDebugging.treeDD.transformations.schemeLambda

import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.deltaDebugging.treeDD.transformations.traits.Replacing
import maf.core.Identifier
import maf.language.scheme.*

object RemoveCalls extends Transformation:
  override val name: String = "RemoveCalls"
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {
    def removeIdCalls(id: Identifier, newTree: SchemeExp): Unit =
      val applsRemoved: Option[SchemeExp] = newTree.deleteChildren({
        case SchemeFuncall(f: SchemeVarExp, _, _) =>
          f.id.name equals id.name
        case _ => false
      })

      applsRemoved match
        case Some(callsRemovedTree) =>
          val trees = Replacing.replaceCallWithAllValues(callsRemovedTree, id) //this removes any non-call references to the lambda
          addTrees(trees)
        case _ =>

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for(lambdaBinding <- lambdaBindings)
          val newTree = tree.replace(exp, exp.dropBinding(lambdaBinding._1.name))
          removeIdCalls(lambdaBinding._1, newTree)

      case defExp@SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
        val newTree = tree.deleteChildren(child => child eq defExp) //removes the definition
        newTree match
          case Some(newTree) =>
            removeIdCalls(name, newTree)
          case _ =>

      case _ =>
  }

