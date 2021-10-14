package maf.modular.incremental.scheme.modf

import maf.core.IdentityMonad
import maf.language.change.CodeVersion.*
import maf.core.*
import maf.language.scheme.*
import maf.modular.incremental.IncrementalGlobalStore
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.modf
import maf.modular.scheme.modf.*
import maf.util.benchmarks.Timeout

/** Implements big-step semantics for an incremental Scheme analysis. * */
trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics with IncrementalGlobalStore[SchemeExp]:

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

        override protected def evalIf(
            prd: SchemeExp,
            csq: SchemeExp,
            alt: SchemeExp
          ): EvalM[Value] =
          for
              prdVal <- eval(prd)
              _ = { implicitFlows = lattice.getAddresses(prdVal) :: implicitFlows }
              adr = implicitFlows.flatten.toSet
              resVal <- cond(prdVal, eval(csq), eval(alt))
              _ = { implicitFlows = implicitFlows.tail }
          yield lattice.addAddresses(resVal, adr)

    override def intraAnalysis(cmp: Component): IncrementalSchemeModFBigStepIntra
