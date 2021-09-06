package maf.modular.adaptive.scheme

import maf.core._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.language.scheme._
import maf.modular.adaptive._

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics
    extends AdaptiveModAnalysis[SchemeExp]
    with AdaptiveGlobalStore[SchemeExp]
    with BaseSchemeModFSemantics
    with SchemeSetup
    with BigStepModFSemantics
    with StandardSchemeModFComponents
    with SchemeModFModules
    with ModularSchemeDomain:
    // Definition of update functions
    def adaptClosure(clo: lattice.Closure): lattice.Closure = clo match
        case (lambda, env: WrappedEnv[Addr, SchemeModule] @unchecked) => (lambda, env.mapAddrs(adaptAddr))
        case _                                                        => throw new Exception(s"Closure with invalid environment: ${clo._2}")
    def adaptAllocCtx(ctx: AllocationContext): AllocationContext
    def adaptAddr(addr: Addr): Addr = addr match
        case ptr: PtrAddr[AllocationContext] @unchecked => PtrAddr(ptr.exp, adaptAllocCtx(ptr.ctx))
        case vad: VarAddr[AllocationContext] @unchecked => VarAddr(vad.id, adaptAllocCtx(vad.ctx))
        case ret: ReturnAddr[Component] @unchecked      => ReturnAddr(adaptComponent(ret.cmp), ret.idn)
        case pad: PrmAddr                               => pad
        case _                                          => throw new Exception(s"Unhandled addr: $addr")
    def adaptValue(value: Value): Value = value match
        case modularLatticeWrapper.modularLattice.Elements(vs) => modularLatticeWrapper.modularLattice.Elements(vs.map(adaptV))
    def adaptV(value: modularLatticeWrapper.modularLattice.Value): modularLatticeWrapper.modularLattice.Value = value match
        case modularLatticeWrapper.modularLattice.Pointer(ps) =>
          modularLatticeWrapper.modularLattice.Pointer(ps.map(adaptAddr))
        case modularLatticeWrapper.modularLattice.Clo(cs) =>
          modularLatticeWrapper.modularLattice.Clo(cs.map(adaptClosure))
        case modularLatticeWrapper.modularLattice.Cons(car, cdr) =>
          modularLatticeWrapper.modularLattice.Cons(adaptValue(car), adaptValue(cdr))
        case modularLatticeWrapper.modularLattice.Vec(siz, els) =>
          modularLatticeWrapper.modularLattice.Vec(siz, els.view.mapValues(adaptValue).toMap)
        case _ => value
    // adapting a component
    def adaptComponent(cmp: Component): Component = cmp match
        case Main                                 => Main
        case c: Call[ComponentContext] @unchecked => adaptCall(c)
    protected def adaptCall(c: Call[ComponentContext]): Call[ComponentContext]
    // go over all new components after each step of the analysis, passing them to `onNewComponent`
    // ensure that these new components are properly updated when an adaptation occurs using a field `toProcess` which is kept up-to-date!
    override def baseEnv = WrappedEnv(initialEnv, 0, MainModule)
    override def intraAnalysis(cmp: Component): AdaptiveSchemeModFIntra = new AdaptiveSchemeModFIntra(cmp)
    class AdaptiveSchemeModFIntra(cmp: Component) extends IntraAnalysis(cmp) with BigStepModFIntra:
        override protected def newClosure(lambda: SchemeLambdaExp, env: Env): Value =
            val trimmedEnv = env.restrictTo(lambda.fv).asInstanceOf[WrappedEnv[Addr, SchemeModule]]
            val updatedEnv = trimmedEnv.copy(depth = trimmedEnv.depth + 1, data = module(cmp))
            lattice.closure((lambda, updatedEnv))
