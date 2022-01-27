package maf.modular.scv

import maf.language.scheme.ContractSchemeProvide
import maf.language.scheme.SchemeExp
import maf.language.scheme.ContractSchemeProvideOut
import maf.language.scheme.ContractSchemeContractOut
import maf.language.scheme.{SchemeFuncall, SchemeVar}
import maf.core.{Identifier, Identity, Monad}

/**
 * Adds support for the (provide ...) special form.
 *
 * The analysis does not feature a full module system, but rather triggers the analysis of the functions listed in the (provide ...) special form.
 *
 * These functions are triggered with the opaque value, and are monitored in their accompanyning contracts before calling them. Note that this also
 * causes a read-dependency to be triggered, so the enclosing component might be re-analyzed multiple times even though we were not really interested
 * in the return value of applying the guarded function with opaque values.
 *
 * Inside the module, the functions listed in the (provide ...) special form will **not** be monitored by the given function.
 *
 * This is consistent with the behaviour of Racket (where this feature orginates from) and the original Nguyen SCV paper.
 */
trait ScvBigStepWithProvides extends BaseScvBigStepSemantics:
    import maf.util.FunctionUtils.*
    import maf.core.Monad.MonadSyntaxOps
    import maf.core.Monad.MonadIterableOps
    import maf.core.MonadStateT.{lift, unlift}
    import evalM._
    import scvMonadInstance.impure

    override def intraAnalysis(component: Component): IntraScvSemanticsWithProvides

    trait IntraScvSemanticsWithProvides extends BaseIntraScvSemantics:
        /** Evaluates a single contract-out expression. Returns the monitored function */
        protected def evalProvideOut(out: ContractSchemeProvideOut): EvalM[(Value, Identifier, Identity)] = out match
            case ContractSchemeContractOut(name, contract, idn) =>
              for
                  function <- extract(evalVariable(name))
                  evaluatedContract <- extract(eval(contract))
                  result <- applyMon(function, evaluatedContract, SchemeVar(name), idn, false, contractExpr = Some(contract))
              yield (result, name, idn)

            case _ => throw new Exception("only contract-out is supported in provide")

        protected def callWithOpq(values: List[(Value, Identifier, Identity)]): EvalM[List[Value]] =
            def fresh(idn: Identity): SchemeExp = SchemeFuncall(SchemeVar(Identifier("fresh", idn)), List(), idn)
            Monad.sequence(values.map { case (value, name, idn) =>
              lattice.getArrs(value).foldLeftM(lattice.bottom) { (vlu, arr) =>
                applyArr(SchemeFuncall(SchemeVar(name), (0 to arr.expectedNumArgs).map(_ => fresh(idn)).toList, idn), PostValue.noSymbolic(arr.e))
                  .map(lattice.join(vlu, _))
              }
            })

        override def eval(exp: SchemeExp): EvalM[Value] = exp match
            case ContractSchemeProvide(outs, idn) =>
              for
                  // first evaluate the names to values and monitor them
                  monitoredFunctions <- Monad.sequence(outs.map(evalProvideOut(_)))
                  // then we apply the ones that are functions with opaque values as arguments, note that this is not the actual semantics of the program, but those functions need to be analyzed seperately
                  _ <- callWithOpq(monitoredFunctions)
              // ignore the results, the effects will be registered in blames if necessary
              yield lattice.nil

            case _ => super.eval(exp)
