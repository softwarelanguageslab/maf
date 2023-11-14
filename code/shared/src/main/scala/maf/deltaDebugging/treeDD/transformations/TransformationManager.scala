package maf.deltaDebugging.treeDD.transformations

import maf.deltaDebugging.treeDD.transformations.schemeSequencify.IfToBegin
import maf.deltaDebugging.treeDD.transformations.schemeLambda.*
import maf.deltaDebugging.treeDD.transformations.schemeBinding.*
import maf.deltaDebugging.treeDD.transformations.generics.*
import maf.deltaDebugging.treeDD.transformations.schemeIdentifier.ReplaceIdentifier
import maf.deltaDebugging.treeDD.transformations.schemePrim.ReplacePrimCalls

object TransformationManager:
  
  val genericTransformations: List[Transformation] =
    List(
      DeleteChildSimple,
      ReplaceByChild
    )

  val allTransformations: List[Transformation] =
    List(
      LetToBegin,
      IfToBegin,
      ThunkToBegin,
      BindingDrop,
      ReplacePrimCalls,
      RemoveLambdaParamWithDeepDrop,
      ReplaceByChild,
      DeleteChildSimple,
      ReplaceCalls,
      BindingReplace,
      RemoveLambdaParamByReplacement,
      ReplaceIdentifier,
      RemoveCalls,
    )
