package maf.TurgutsThesis.gtr.transformations

object TransformationManager:

  val allTransformations: List[Transformation] =
    List(
      DeleteChildSimple,
      DropLetIdentifier,
      IfToBegin,
      RemoveCalls,
      RemoveCallsAndReplaceByBody,
      RemoveLambdaParamByReplacement,
      RemoveLambdaParamWithDeepDrop,
      RemoveLambdaParamWithShallowDrop,
      ReplaceByChild,
      ReplaceIdentifier,
      ReplaceIdentifierCalls,
    )
