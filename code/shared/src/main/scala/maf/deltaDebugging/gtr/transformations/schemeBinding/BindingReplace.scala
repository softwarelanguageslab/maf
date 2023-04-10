package maf.deltaDebugging.gtr.transformations.schemeBinding

import maf.deltaDebugging.gtr.transformations.Transformation
import maf.deltaDebugging.gtr.transformations.traits.Replacing
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, RacketModule, RacketModuleExpose, RacketModuleLoad, RacketProvide, RacketRequire, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

object BindingReplace extends Transformation:
  override val name: String = "BindingReplace"

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettish: SchemeLettishExp =>
        for (id <- lettish.bindings.map(_._1))
          val bindingDropped = lettish.dropBinding(id.name)
          val lets = Replacing.replaceIdWithAllValues(bindingDropped, id)
          addReplacements(lets)
      case _ =>

