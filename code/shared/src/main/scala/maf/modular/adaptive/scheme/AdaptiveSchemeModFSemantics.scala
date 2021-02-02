package maf.modular.adaptive.scheme

import maf.core._
import maf.modular._
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.language.scheme._
import maf.modular.adaptive._

/** Semantics for an adaptive Scheme MODF analysis. */
trait AdaptiveSchemeModFSemantics
    extends AdaptiveModAnalysis[SchemeExp]
       with AdaptiveGlobalStore[SchemeExp]
       with SchemeModFSemantics
       with BigStepModFSemantics
       with ModularSchemeDomain {
  // Definition of components
  type ComponentData = SchemeModFComponent
  lazy val initialComponentData = Main 
  // Need init to initialize reference bookkeeping information.
  def newComponent(call: Call): Component = ref(call)
  // Definition of update functions
  def updateClosure(update: Component => Component)(clo: lattice.Closure) = clo match {
    case (lambda, env: WrappedEnv[Addr, Component] @unchecked) => (lambda, env.copy(data = update(env.data)).mapAddrs(updateAddr(update)))
  }
  def updateCmp(update: Component => Component)(cmp: ComponentData): ComponentData = cmp match {
    case Main                                  => Main
    case Call(clo, nam, ctx: ComponentContext) => Call(updateClosure(update)(clo), nam, updateCtx(update)(ctx))
  }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext
  def updateAddr(update: Component => Component)(addr: Addr): Addr = addr match {
    case ptr: PtrAddr[Component] @unchecked    => PtrAddr(ptr.exp, update(ptr.ctx))
    case vad: VarAddr[Component] @unchecked    => VarAddr(vad.id, update(vad.ctx))
    case ret: ReturnAddr[Component] @unchecked => ReturnAddr(update(ret.cmp), ret.idn)
    case pad: PrmAddr                          => pad
    case _                                     => throw new Exception(s"Unhandled addr: $addr")
  }
  def updateValue(update: Component => Component)(value: Value): Value = value match {
    case modularLatticeWrapper.modularLattice.Elements(vs) => modularLatticeWrapper.modularLattice.Elements(vs.map(updateV(update)))
  }
  def updateV(update: Component => Component)(value: modularLatticeWrapper.modularLattice.Value): modularLatticeWrapper.modularLattice.Value =
    value match {
      case modularLatticeWrapper.modularLattice.Pointer(ps) => modularLatticeWrapper.modularLattice.Pointer(ps.map(updateAddr(update)))
      case modularLatticeWrapper.modularLattice.Clo(cs) =>
        modularLatticeWrapper.modularLattice.Clo(cs.map(clo => (updateClosure(update)(clo._1), clo._2)))
      case modularLatticeWrapper.modularLattice.Cons(car, cdr) =>
        modularLatticeWrapper.modularLattice.Cons(updateValue(update)(car), updateValue(update)(cdr))
      case modularLatticeWrapper.modularLattice.Vec(siz, els) =>
        modularLatticeWrapper.modularLattice.Vec(siz, els.view.mapValues(updateValue(update)).toMap)
      case _ => value
    }
  // adapting a component
  def adaptComponent(cmp: ComponentData): ComponentData = cmp match {
    case Main               => Main
    case c: Call @unchecked => adaptCall(c)
  }
  protected def adaptCall(c: Call): Call
  // callback function that can adapt the analysis whenever a new component is 'discovered'
  protected def onNewComponent(cmp: Component, call: Call): Unit = ()
  // go over all new components after each step of the analysis, passing them to `onNewComponent`
  // ensure that these new components are properly updated when an adaptation occurs using a field `toProcess` which is kept up-to-date!
  var toProcess = Set[Component]()
  override protected def adaptAnalysis() = {
    this.toProcess = this.newComponents
    while (toProcess.nonEmpty) {
      val cmp = toProcess.head
      toProcess = toProcess.tail
      val call = view(cmp).asInstanceOf[Call]
      onNewComponent(cmp, call)
    }
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.toProcess = updateSet(update)(toProcess)
  }
  override def baseEnv = WrappedEnv(super.baseEnv, 0, mainComponent)
  override def intraAnalysis(cmp: Component): AdaptiveSchemeModFIntra = new AdaptiveSchemeModFIntra(cmp)
  class AdaptiveSchemeModFIntra(cmp: Component) extends IntraAnalysis(cmp) with BigStepModFIntra with DependencyTrackingIntra {
    override protected def newClosure(
        lambda: SchemeLambdaExp,
        env: Env,
        name: Option[String]
      ): Value = {
      val trimmedEnv = env.restrictTo(lambda.fv).asInstanceOf[WrappedEnv[Addr, Component]]
      val updatedEnv = trimmedEnv.copy(depth = trimmedEnv.depth + 1, data = component)
      lattice.closure((lambda, updatedEnv), name)
    }
  }
}
