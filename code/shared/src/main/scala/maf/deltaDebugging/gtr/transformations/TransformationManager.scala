package maf.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeSequencify.{ApplToBegin, IfToBegin}
import maf.deltaDebugging.gtr.transformations.schemeLambda.*
import maf.deltaDebugging.gtr.transformations.schemeLet.*
import maf.deltaDebugging.gtr.transformations.generics.*
import maf.deltaDebugging.gtr.transformations.schemeIdentifier.ReplaceIdentifier
import maf.deltaDebugging.gtr.transformations.schemePrim.ReplacePrimCalls

object TransformationManager:
  
  val genericTransformations: List[Transformation] =
    List(
      DeleteChildSimple,
      ReplaceByChild
    )

  val allTransformations: List[Transformation] =
    List(
      RemoveLambdaParamWithShallowDrop,
      IfToBegin,
      LetIdentifierShallowDrop,
      ApplToBegin,
      ReplacePrimCalls,
      //LetIdentifierDeepDrop,
      //RemoveLambdaParamWithDeepDrop,
      FlattenThunk,
      ReplaceByChild,
      DeleteChildSimple,
      ReplaceIdentifier,
      ReplaceCalls,
      ReplaceByValue,
      RemoveCalls
    )
