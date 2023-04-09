package maf.deltaDebugging.gtr.transformations

import maf.deltaDebugging.gtr.transformations.schemeSequencify.IfToBegin
import maf.deltaDebugging.gtr.transformations.schemeLambda.*
import maf.deltaDebugging.gtr.transformations.schemeLet.*
import maf.deltaDebugging.gtr.transformations.generics.*
import maf.deltaDebugging.gtr.transformations.schemeDefine.{DefineDrop, DefineReplace}
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
      IfToBegin,
      ReplacePrimCalls,
      LetIdentifierReplace,
      LetIdentifierDeepDrop,
      RemoveLambdaParamWithDeepDrop,
      RemoveLambdaParamByReplacement,
      FlattenThunk,
      ReplaceByChild,
      DeleteChildSimple,
      ReplaceIdentifier,
      ReplaceCalls,
      RemoveCalls,
      LetToBegin,
      DefineDrop,
      DefineReplace,
    )
