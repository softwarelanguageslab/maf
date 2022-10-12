package maf.modular.df.scheme

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.language.scheme.*
import maf.language.sexp
import maf.modular.df.*
import maf.modular.incremental.scheme.lattice.*
import maf.modular.scheme.*
import maf.modular.scheme.modf.*
import maf.util.benchmarks.Timeout
import maf.modular.*

/** Implements big-step semantics for a Scheme DF analysis. * */
trait SchemeModFBigStepDFSemantics extends ModAnalysis[SchemeExp] with BigStepModFSemantics with IncrementalSchemeDomain with GlobalStoreDF[SchemeExp]:

    override def warn(msg: String): Unit = ()

    trait SchemeModFBigStepDFIntra extends IntraAnalysis with BigStepModFIntra with GlobalStoreDFIntra:

        /**
         * Evaluation of a conditional that handles implicit value flows.
         * @note
         *   See [Liu et al. 2010].
         */
        override protected def evalIf(prd: SchemeExp, csq: SchemeExp, alt: SchemeExp): EvalM[Value] =
            for
                prdVal <- eval(prd)
                // Implicit flows go from the predicate to the branches of the conditional.
                _ = { implicitFlows = lattice.getAddresses(prdVal) :: implicitFlows }
                adr = implicitFlows.flatten.toSet
                resVal <- cond(prdVal, eval(csq), eval(alt))
                _ = { implicitFlows = implicitFlows.tail }
            // Implicit flows need to be added to the return value of the if as well, as this value depends on the predicate.
            yield lattice.addAddresses(resVal, adr)

    /*
    /** Evaluation of a literal value that adds a "literal address" as source. */
    override protected def evalLiteralValue(literal: sexp.Value, exp: SchemeExp): M[Value] =
        // sorry for the Monad, Jens...
        if configuration.cyclicValueInvalidation then
            val a = LitAddr(exp)
            for value <- super.evalLiteralValue(literal, exp).map(lattice.addAddress(_, a)) // Attach the address to the value for flow tracking.
            // _ = { if !lattice.isBottom(value) then intraProvenance += (a -> value) } // We can just overwrite any previous value as it will be the same.
            yield value
        else super.evalLiteralValue(literal, exp)
     */

    override def intraAnalysis(cmp: Component): SchemeModFBigStepDFIntra

    override def configString(): String = super.configString() + "\n  applying incremental big-step ModF Scheme semantics"
