package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifierCalls.{addReplacements, addTrees, replaceCallWithAllValues, replaceWithAllValues}
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.TurgutsThesis.primitiveOpNames.PrimitiveOpNames
import maf.core.Identity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

class ReplaceNthExpensiveFunction(arr: Array[(String, Int)], n: Int) extends Transformation with Replacing:
  override val name: String = "ReplaceNthExpensiveFunction"
  private val nthExpensive = arr(n)

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case lettishExp: SchemeLettishExp =>
        val lambdaBindings = lettishExp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for (lambdaBinding <- lambdaBindings)
          if lambdaBinding._1.name equals nthExpensive._1 then 
            addReplacements(replaceCallWithAllValues(lettishExp, lambdaBinding._1))

      case SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
        if name.name equals nthExpensive._1 then 
          addTrees(replaceCallWithAllValues(tree, name))

      case _ =>