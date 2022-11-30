package maf.TurgutsThesis.gtr.transformations
import maf.language.scheme.{AContractSchemeMessage, ASchemeExp, CSchemeExp, ContractSchemeExp, MatchExpr, SchemeAssert, SchemeBegin, SchemeCodeChange, SchemeDefineVariable, SchemeExp, SchemeFuncall, SchemeIf, SchemeLambdaExp, SchemeLettishExp, SchemeSetExp, SchemeValue, SchemeVarExp, SymbolicHole, SymbolicVar}
import maf.core.Identifier

object RemoveCalls extends Transformation:
  override val name: String = "RemoveCalls"
  override def transformAndAdd(tree: SchemeExp, node: SchemeExp): Unit = {

    def removeIdCalls(id: Identifier): Unit =
      val applsRemoved: Option[SchemeExp] = tree.deleteChildren({
        case SchemeFuncall(f: SchemeVarExp, _, _) =>
          f.id.name equals id.name
        case _ => false
      })

      applsRemoved match
        case Some(tree) =>
          addTree(tree)
        case _ =>

    node match
      case exp: SchemeLettishExp =>
        val lambdaBindings = exp.bindings.collect({
          case (identifier, lambda: SchemeLambdaExp) => (identifier, lambda)
        })

        for(lambdaBinding <- lambdaBindings)
          removeIdCalls(lambdaBinding._1)

      case SchemeDefineVariable(name, _: SchemeLambdaExp, _) =>
        removeIdCalls(name)

      case _ =>
  }

