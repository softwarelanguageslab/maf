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
      IfToBegin,
      RemoveLambdaParamWithShallowDrop,
      LetIdentifierShallowDrop,
      ReplaceByChild,
      RemoveLambdaParamWithDeepDrop,
      LetIdentifierDeepDrop,
      RemoveCallsAndReplaceByBody,
      DeleteChildSimple,
      ReplaceIdentifier,
      ReplaceBySimpleValue,
      ReplaceByValue,
      ReplaceCalls,
      RemoveCalls
    )
