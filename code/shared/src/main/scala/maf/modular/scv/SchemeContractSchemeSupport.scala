package maf.modular.scv

import maf.modular.scheme.modf.BigStepModFSemantics
import maf.language.ContractScheme.StructOps
import maf.language.scheme.*
import scala.reflect.ClassTag
import maf.core.Position.Position
import maf.core.Monad
import maf.language.scheme.primitives.MF

/**
 * This trait introduces support for contract scheme expressions on top of the big step semantics of Scheme by representing them as no-ops.
 *
 * For example a (mon contract expr) ingores the contract and simply evaluates expr.
 */
trait SchemeContractSchemeSupport extends BigStepModFSemantics:
    import EvalM.*

    protected val valueClassTag: ClassTag[Value]

    override def intraAnalysis(component: Component): BigStepModFIntraSupport

    trait BigStepModFIntraSupport extends BigStepModFIntra:
        private given ClassTag[Value] = valueClassTag
        private val structOps = StructOps[Value]()

        override def eval(exp: SchemeExp): EvalM[Value] = exp match
            // Dependent contracts evaluate to nil
            case ContractSchemeDepContract(_, _, _) => unit(lattice.nil)
            // Flat contracts simply evaluate to their internal expression
            case ContractSchemeFlatContract(expr, _) => eval(expr)
            /** The monitor of an expression simply evaluates to that expression */
            case ContractSchemeMon(_, exp, _) => eval(exp)
            /** Flat contracts evaluate to their inner value, so check can be translated to simple function applications and then evaluated */
            case ContractSchemeCheck(contract, valueExpression, idn) =>
              eval(SchemeFuncall(contract, List(valueExpression), idn))
            /** Provide expressions are ignored and evaluate to nil */
            case ContractSchemeProvide(_, _) => unit(lattice.nil)
            /** Struct semantics are preserved */
            case mk: MakeStruct => unit(structOps.evaluate(mk))
            case MatchExpr(value, clauses, _) =>
              for
                  _ <- eval(value) // evaluate value for side effects but ignore result
                  evaluatedClauses <- merge(clauses.map(_.expr).map(evalSequence)) // over approximate by evaluating all clauses at once
              yield evaluatedClauses
            case _ => super.eval(exp)

        override def applyFun(
            fexp: SchemeFuncall,
            fval: Value,
            args: List[(SchemeExp, Value)],
            cll: Position,
            ctx: ContextBuilder = DefaultContextBuilder,
          ): EvalM[Value] =
            /** Inject the semantics for structs */
            import maf.core.Monad.MonadSyntaxOps
            import maf.language.scheme.primitives.MFInstance

            val structVals: List[MF[Value]] = structOps.call(fval, args.map(_._2)).toList
            val structVal = Monad.merge(structVals).getOrElse(lattice.bottom)
            super.applyFun(fexp, fval, args, cll).map(lattice.join(_, structVal))

end SchemeContractSchemeSupport
