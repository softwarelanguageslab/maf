package maf.TurgutsThesis.gtr.transformations

import maf.TurgutsThesis.gtr.transformations.ReplaceIdentifierCalls.replaceWithAllValues
import maf.TurgutsThesis.gtr.transformations.traits.Replacing
import maf.TurgutsThesis.primitiveOpNames.PrimitiveOpNames
import maf.core.Identity
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambda, SchemeLambdaExp, SchemeLettishExp, SchemeSanitizer, SchemeSetExp, SchemeSink, SchemeSource, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}

class ReplaceNthExpensiveFunction(arr: Array[(String, Int)], n: Int) extends Transformation with Replacing:
  override val name: String = "RemoveNthExpensiveFunction"
  private val nthExpensive = arr(n)

  override protected def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit =
    node match
      case exp: SchemeLambdaExp =>
        exp.name match
          case Some(lambdaName) =>
            if lambdaName equals nthExpensive._1 then
              val lambdaDeleted: Option[SchemeExp] = tree.deleteChildren(subExp => {
                subExp eq exp
              })

              lambdaDeleted match
                case Some(tree) =>
                  val suggestions = replaceWithAllValues(tree, subExp => {
                    subExp match
                      case SchemeFuncall(f: SchemeVarExp, _, _) =>
                        f.id.name equals lambdaName
                      case varExp: SchemeVarExp => varExp.id.name equals lambdaName
                      case _ => false
                  })

                  addTrees(suggestions)
                case _ =>
          case _ =>
      case _ =>
