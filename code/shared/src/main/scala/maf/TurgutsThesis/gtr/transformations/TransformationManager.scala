package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.schemeIf.IfToBegin
import maf.TurgutsThesis.gtr.transformations.schemeLambda.*
import maf.TurgutsThesis.gtr.transformations.schemeLet.*
import maf.TurgutsThesis.gtr.transformations.generics.*
import maf.TurgutsThesis.gtr.transformations.schemeIdentifier.ReplaceIdentifier

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
      ReplaceIdentifier,
      ReplaceByValue,
    )
