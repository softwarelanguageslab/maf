package maf.deltaDebugging.gtr.transformations.schemeSequencify

import maf.core.NoCodeIdentity
import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, RacketModule, RacketModuleExpose, RacketModuleLoad, RacketProvide, RacketRequire, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object ApplToBegin extends Transformation:
  override val name: String = "ApplToBegin"

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeFuncall(f, args, idn) =>
        addReplacement(
          SchemeBegin(
            List(f) ++ args,
            NoCodeIdentity
          )
        )
      case _ =>
        
