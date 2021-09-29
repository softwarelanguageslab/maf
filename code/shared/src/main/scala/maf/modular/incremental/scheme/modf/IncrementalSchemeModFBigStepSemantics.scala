package maf.modular.incremental.scheme.modf

import maf.language.change.CodeVersion.*
import maf.language.scheme.*
import maf.modular.incremental.IncrementalGlobalStore
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.modf.EvalM.*
import maf.modular.scheme.modf.*

/** Implements big-step semantics for an incremental Scheme analysis. * */
trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics with IncrementalGlobalStore[SchemeExp]:

    override protected def cond(prd: Value, csq: => EvalM[Value], alt: => EvalM[Value]): EvalM[Value] =
        //implicitFlows = lattice.getAddresses(prd) :: implicitFlows
        val csqValue = guard(lattice.isTrue(prd)).flatMap(_ => csq)
        val altValue = guard(lattice.isFalse(prd)).flatMap(_ => alt)
        //implicitFlows = implicitFlows.tail
        merge(csqValue, altValue).map(v => lattice.addAddresses(v, lattice.getAddresses(prd)))

    trait IncrementalSchemeModFBigStepIntra extends BigStepModFIntra with IncrementalIntraAnalysis:
        override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
            case SchemeCodeChange(e, _, _) if version == Old =>
              registerComponent(e, component)
              eval(e) // This could also be a super call if we assume no nesting of change expressions (which could be expected).
            case SchemeCodeChange(_, e, _) if version == New =>
              registerComponent(e, component)
              eval(e) // Same than above.
            case _ =>
              registerComponent(exp, component)
              super.eval(exp)
