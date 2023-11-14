package maf.deltaDebugging.treeDD.transformations.schemePrim

import maf.core.NoCodeIdentity
import maf.deltaDebugging.treeDD.transformations.Transformation
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, RacketModule, RacketModuleExpose, RacketModuleLoad, RacketProvide, RacketRequire, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.language.sexp.Value

object ReplacePrimCalls extends Transformation:
  override val name: String = "FoldPrimitives"

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case SchemeFuncall(f: SchemeVarExp, args, idn) =>
        val name = f.id.name
        if name.endsWith("?") then
          addReplacements(
            List(
              SchemeValue(Value.Boolean(true), NoCodeIdentity),
              SchemeValue(Value.Boolean(false), NoCodeIdentity),
            )
          )
        else if name.contains("string->number") then
          addReplacements(
            List(
              SchemeValue(Value.Integer(1), NoCodeIdentity),
              SchemeValue(Value.Integer(0), NoCodeIdentity)
            )
          )
        else if name.contains("number->string") then
          addReplacements(
            List(
              SchemeValue(Value.String("1"), NoCodeIdentity),
              SchemeValue(Value.String("0"), NoCodeIdentity),
            )
          )
        else if name.contains("string->symbol") then
          addReplacement(
            SchemeValue(Value.Symbol("a"), NoCodeIdentity),
          )
        else if name.contains("symbol->string") then
          addReplacement(
            SchemeValue(Value.String("a"), NoCodeIdentity),
          )
      case _ =>