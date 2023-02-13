package maf.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeIf.IfToBegin
import maf.deltaDebugging.gtr.transformations.schemeLambda.*
import maf.deltaDebugging.gtr.transformations.schemeLet.*
import maf.deltaDebugging.gtr.transformations.generics.*
import maf.deltaDebugging.gtr.transformations.schemeIdentifier.ReplaceIdentifier

object TransformationManager:
  
  val genericTransformations: List[Transformation] =
    List(
      DeleteChildSimple,
      ReplaceByChild
    )

  val allTransformations: List[Transformation] =
    List(
      DeleteChildSimple,
      IfToBegin,
      LetIdentifierDeepDrop,
      LetIdentifierShallowDrop,
      RemoveCalls,
      RemoveCallsAndReplaceByBody,
      RemoveLambdaParamByReplacement,
      RemoveLambdaParamWithDeepDrop,
      RemoveLambdaParamWithShallowDrop,
      ReplaceByChild,
      ReplaceCalls,
      ReplaceBySimpleValue,
      ReplaceIdentifier,
      ReplaceByValue
    )
