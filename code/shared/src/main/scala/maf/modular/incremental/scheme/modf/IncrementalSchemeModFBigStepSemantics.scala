package maf.modular.incremental.scheme.modf

import maf.language.change.CodeVersion.*
//import maf.core.*
import maf.language.scheme.*
import maf.modular.incremental.IncrementalGlobalStore
import maf.modular.incremental.scheme.IncrementalSchemeSemantics
import maf.modular.incremental.scheme.lattice.IncrementalAbstractDomain
import maf.modular.scheme.modf.EvalM.*
import maf.modular.scheme.modf.*

/** Implements big-step semantics for an incremental Scheme analysis. * */
trait IncrementalSchemeModFBigStepSemantics extends BigStepModFSemantics with IncrementalSchemeSemantics with IncrementalGlobalStore[SchemeExp]:

    /*
    case class State(stack: List[Set[Addr]]):
        def pop(): State = State(stack.tail)
        def push(annotations: Set[Addr]): State = State(annotations :: stack)

    override type EvalM[X] = MonadStateT[State, IdentityMonad, X]
    implicit val evalM: TEvalM[EvalM] = new TEvalM:
        val stateInstance = MonadStateT.stateInstance[State, IdentityMonad]

    override protected def cond(prd: Value, csq: => EvalM[Value], alt: => EvalM[Value]): EvalM[Value] =
        implicitFlows = lattice.getAddresses(prd) :: implicitFlows
        println("iflow: " + implicitFlows.mkString(" -- "))
        val csqValue = guard(lattice.isTrue(prd)).flatMap(_ => csq)
        val altValue = guard(lattice.isFalse(prd)).flatMap(_ => alt)
        val r = merge(csqValue, altValue) //.map(v => lattice.addAddresses(v, implicitFlows.flatten.toSet))
        implicitFlows = implicitFlows.tail
        println("iflow - " + prd + "  " + csq + "  " + alt)
        r
     */

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
