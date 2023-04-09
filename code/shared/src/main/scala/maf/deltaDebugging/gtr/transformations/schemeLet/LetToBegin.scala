package maf.deltaDebugging.gtr.transformations.schemeLet

import maf.core.NoCodeIdentity
import maf.deltaDebugging.gtr.transformations.Transformation
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, RacketModule, RacketModuleExpose, RacketModuleLoad, RacketProvide, RacketRequire, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object LetToBegin extends Transformation:
  override val name: String = "LetToBegin"

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLettishExp =>
        if exp.bindings.isEmpty then
          addReplacement(SchemeBegin(exp.body, NoCodeIdentity))
      case _ =>
