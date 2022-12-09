package maf.TurgutsThesis.gtr.transformations

object TransformationManager:

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
      ReplaceIdentifier,
      ReplaceCalls,
    )
