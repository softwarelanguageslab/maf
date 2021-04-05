package maf.modular.adaptive.scheme

import maf.core._
import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.util.datastructures._
import maf.modular.scheme._
import maf.modular._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.Monoid
import maf.util.MonoidImplicits._
import maf.util.MonoidInstances
import maf.modular.components.ComponentPointer

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics {

  import modularLatticeWrapper.modularLattice.{schemeLattice => lat}

  // configured by (TODO: in Scala 3, turn these into trait parameters):
  // - a threshold `n` for the number of components per module
  // - a threshold `t` for how many times a component is triggered before adaptation
  val n: Int
  val t: Int

  // disable warning messages and debug logging by default (can be override for custom logging)
  protected def warn(message: => String): Unit = ()
  protected def debug(message: => String): Unit = ()

  // context-sensitivity policy can be configured per closure
  // choice of policies is left open as a parameter; the following needs to be provided:
  // - a starting policy `defaultPolicy` (with high precision) 
  // - a `nextPolicy` method, which computes for a given closure the next policy (with lower precision)
  // - a `glbPolicy` method that computes the most precise policy that is less precise than both given policies

  trait ContextSensitivityPolicy {
    def allocCtx(
      clo: lat.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext
    def adaptCtx(ctx: ComponentContext): ComponentContext
  }

  val defaultPolicy: ContextSensitivityPolicy
  def nextPolicy(clo: lat.Closure, 
                 cur: ContextSensitivityPolicy,
                 cts: Set[ComponentContext]): ContextSensitivityPolicy
  def glbPolicy(pl1: ContextSensitivityPolicy, pl2: ContextSensitivityPolicy): ContextSensitivityPolicy

  private implicit val policyMonoid = new Monoid[ContextSensitivityPolicy] {
    def zero = defaultPolicy
    def append(pl1: ContextSensitivityPolicy, pl2: => ContextSensitivityPolicy) = glbPolicy(pl1,pl2)
  }

  private var policyPerClosure: Map[lat.Closure, ContextSensitivityPolicy] = Map.empty
  def getCurrentPolicy(clo: lat.Closure): ContextSensitivityPolicy = 
    policyPerClosure.getOrElse(clo, defaultPolicy)

  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    getCurrentPolicy(clo).allocCtx(clo, args, call, caller)
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    cll.copy(ctx = getCurrentPolicy(cll.clo).adaptCtx(cll.ctx))

  // modules (in Scheme), which are either:
  // - the main module (~ top-level code)
  // - a lambda module (~ function definition)

  trait SchemeModule
  case object MainModule extends SchemeModule {
    override def toString = "main"
  }
  case class LambdaModule(lambda: SchemeLambdaExp) extends SchemeModule {
    override def toString = lambda.lambdaName
  }

  def module(cmp: Component): SchemeModule = view(cmp) match {
    case Main         => MainModule
    case Call(clo, _) => module(clo)
  }
  def module(clo: lat.Closure): LambdaModule = LambdaModule(clo._1)

  // data kept by the analysis, includes:
  // - the components per module
  // - the dependencies read by a component
  // - the number of times a dependency is triggered
  // - the number of times a component is triggered
  var cmpsPerModule: Map[SchemeModule, Set[Component]] = Map.empty
  var depsPerComponent: Map[Component, Set[Dependency]] = Map.empty
  var depTriggerCounts: Map[Dependency, Int] = Map.empty
  var cmpTriggerCounts: Map[Component, Int] = Map.empty

  override def spawn(cmp: Component) = {
    if (!visited(cmp)) {
      val mod = module(cmp)
      val updatedCmps = cmpsPerModule.getOrElse(mod, Set.empty) + cmp
      cmpsPerModule += mod -> updatedCmps
      if (updatedCmps.size == n) {
        markedModules += mod
      }
    }
    super.spawn(cmp)
  }

  override def register(cmp: Component, dep: Dependency) = {
    depsPerComponent += cmp -> (depsPerComponent(cmp) + dep)
    super.register(cmp, dep)
  }

  override def trigger(dep: Dependency) = {
    depTriggerCounts += dep -> (depTriggerCounts.getOrElse(dep, 0) + 1)
    deps.getOrElse(dep, Set.empty).foreach { cmp =>
      val updatedCount = cmpTriggerCounts.getOrElse(cmp, 0) + 1
      cmpTriggerCounts += cmp -> updatedCount
      if (updatedCount == t) {
        markedComponents += cmp
      }
    }
    super.trigger(dep)
  }

  // before adaptation, keep track of ... 
  // - which modules have too many components
  // - which components are triggered too often

  private var markedModules: Set[SchemeModule] = Set.empty
  private var markedComponents: Set[Component] = Set.empty 

  // ... during adaptation (to avoid duplicating work), keep track of ...
  // - for which modules the number of closures has been reduced
  // - for which dependencies the value abstraction has been reduced

  private var reducedModules: Set[SchemeModule] = Set.empty
  private var reducedDependencies: Set[Dependency] = Set.empty

  // adapting the analysis
  // - for marked modules => reduce the number of components
  // - for marked components => reduce how many times the component is triggered

  def adaptAnalysis() = {
    // start the adaptation
    markedModules.foreach { mod => 
      reduceComponentsForModule(mod)
    }
    markedComponents.foreach { cmp =>
      reduceReanalysesForComponent(cmp)
      cmpTriggerCounts -= cmp 
    }
    // reset the count for all "reduced" dependencies
    reducedDependencies.foreach { dep => 
      depTriggerCounts -= dep
    }
    // update the analysis
    if (reducedModules.nonEmpty) { updateAnalysis() }
    // clear the set of reduced modules and dependencies
    reducedModules = Set.empty
    reducedDependencies = Set.empty
    // unmark all modules and components
    markedModules = Set.empty
    markedComponents = Set.empty
  }

  // extra parameters to control the "aggressiveness" of the adaptation (TODO: in Scala 3, make these (default?) trait parameters):
  // - `reduceFactor`: determines by what factor the number of components needs to be reduced 
  // - `cutoffFactor`: determines the cutoff for selecting "culprits" to be reduced in the adaptation
  val reduceFactor = 0.25
  val cutoffFactor = 0.25

  private def reduceComponentsForModule(module: SchemeModule) = module match {
    case MainModule      => warn("Attempting to reduce the number of components for the main module")
    case l: LambdaModule => reduceComponents(l)
  }

  private def reduceComponents(module: LambdaModule): Unit =
    if (!reducedModules(module)) { // ensure this is only done once per module per adaptation
      reducedModules += module
      // get all components, calls and closures for this module
      val cmps = cmpsPerModule(module)
      val calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]]).toSet
      val callsPerClo = calls.groupBy[lat.Closure](_.clo)
      // look at the number of closures vs contexts
      val numberOfClosures = callsPerClo.size
      val maxContextsPerClosure = callsPerClo.maxBy(_._2.size)._2.size
      if (numberOfClosures > maxContextsPerClosure) {
        reduceClosuresForFunction(module.lambda, callsPerClo.keySet)
      } else {
        val selected = selectLargest[(lat.Closure, Set[Call[ComponentContext]])](callsPerClo, _._2.size, maxContextsPerClosure)
        selected.foreach { case (clo, calls) => reduceContext(clo, calls) }
      }
    }

  private def reduceContext(closure: lat.Closure, calls: Set[Call[ComponentContext]]): Unit = {
    if (calls.size == 1) { // can't do anything if there is only one component
      warn(s"Attempting to reduce contexts for a single call ${calls.head}")
      return
    }
    // find a fitting policy
    var contexts = calls.map(_.ctx)
    var policy = getCurrentPolicy(closure)
    val target = Math.max(1, calls.size * reduceFactor)
    while (contexts.size > target) { // TODO: replace with binary search?
      policy = nextPolicy(closure, policy, contexts)
      contexts = contexts.map(policy.adaptCtx)
    }
    // register the new policy
    debug(s"${printClosure(closure)} -> $policy")
    policyPerClosure += closure -> policy 
  }


  private def reduceClosures(cls: Set[lat.Closure]) = {
    val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
    val selected = selectLargest[(SchemeLambdaExp, Set[lat.Closure])](groupByFunction, _._2.size)
    selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }
  }

  private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[lat.Closure]) =
    reduceComponentsForModule(getParentModule(closures.head))

  def selectLargest[D](data: Iterable[D], size: D => Int): Iterable[D] =
    selectLargest(data, size, size(data.maxBy(size)))
  def selectLargest[D](
      data: Iterable[D],
      size: D => Int,
      max: Int
    ): Iterable[D] = {
    val target = max * cutoffFactor
    data.filter(size(_) > target)
  }

  // look at all the components that were triggered too often
  private def reduceReanalysesForComponent(cmp: Component) = {
    val deps = depsPerComponent(cmp)
    val depsByLocation = deps.groupBy { case AddrDependency(addr) => getAddrExp(addr) }
    val depsByLocationWithCounts = depsByLocation.map { case (loc, dps) => 
      (loc, (dps, dps.map(dep => depTriggerCounts.getOrElse(dep, 0)).sum))
    }
    val numberOfLocations = depsByLocationWithCounts.filter
    val maxLocationCount = depsByLocationWithCounts.map(_._2._2).max
    if(numberOfLocations > maxLocationCount) {
      warn("Nothing we can do about this ... yet!")
    } else {
      val selected = selectLargest[(Expression, Set[(Dependency,Int)])](depsWithCountsByLocation, _._2.size, maxLocationCount)
      selected.foreach { case (loc, deps) => reduceDependenciesForLocation(loc, deps) }
    }
  }

  // assumes all dependencies in `deps` correspond to a single program location!
  private def reduceDependenciesForLocation(loc: Expression, deps: MultiSet[Dependency]): Unit = {
    val numberOfDependencies = deps.distinctCount
    val maximumDependencyCost = deps.content.maxBy(_._2)._2
    if (numberOfDependencies > maximumDependencyCost) {
      reduceAddressesForLocation(loc, deps.toSet.map(_.asInstanceOf[AddrDependency].addr))
    } else {
      val selected = selectLargest[(Dependency, Int)](deps.content, _._2, maximumDependencyCost)
      selected.foreach { case (dep, _) => reduceDep(dep) }
    }
  }

  private def reduceDep(dep: Dependency) =
    if (!reducedDependencies(dep)) {
      reducedDependencies += dep
      dep match {
        case AddrDependency(addr) => reduceValueAbs(store(addr))
        case _                    => throw new Exception("Unknown dependency for adaptive analysis")
      }
    }

  private def reduceValueAbs(value: Value): Unit = value.vs.maxBy(sizeOfV) match {
    case modularLatticeWrapper.modularLattice.Pointer(pts) => reduceAddresses(pts)
    case modularLatticeWrapper.modularLattice.Clo(cls)     => reduceClosures(cls)
    case modularLatticeWrapper.modularLattice.Cons(car, cdr) =>
      if (sizeOfValue(car) > sizeOfValue(cdr)) {
        reduceValueAbs(car)
      } else {
        reduceValueAbs(cdr)
      }
    case modularLatticeWrapper.modularLattice.Vec(_, elms) =>
      val value = elms.map(_._2).maxBy(sizeOfValue) // assume elms is not empty!
      reduceValueAbs(value)
    case v => warn(s"Attempting to adapt value a non-set-based value $v")
  }

  private def reduceAddresses(addrs: Set[Addr]) = {
    val groupByLocation = addrs.groupBy[Expression](getAddrExp)
    val selected = selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)
    selected.foreach { case (loc, addrs) => reduceAddressesForLocation(loc, addrs) }
  }

  private def reduceAddressesForLocation(loc: Expression, addrs: Set[Addr]) =
    reduceComponentsForModule(getAddrModule(addrs.head))

  // updating the analysis

  override def updateAnalysisData(update: Map[Component,Component]) = {
    super.updateAnalysisData(update)
    this.cmpsPerModule = updateMap(updateSet(update))(cmpsPerModule)
    this.depsPerComponent = updateMap(update, updateSet(updateDep(update)))(depsPerComponent)
    this.depTriggerCounts = updateMap(updateDep(update), (c: Int) => c)(depTriggerCounts)(MonoidInstances.intMaxMonoid)
    this.cmpTriggerCounts = updateMap(update, (c: Int) => c)(cmpTriggerCounts)(MonoidInstances.intMaxMonoid)
    this.policyPerClosure = updateMap(updateClosure(update), (p: ContextSensitivityPolicy) => p)(policyPerClosure)
  }

  /*
   * HELPERS
   * (TODO: factor our some of these ...)
   */
  private def getAddrCmp(addr: Addr): Component = addr match {
    case returnAddr: ReturnAddr[Component] @unchecked => returnAddr.cmp
    case schemeAddr: SchemeAddr[Component] @unchecked =>
      schemeAddr match {
        case VarAddr(_, cmp) => cmp
        case PtrAddr(_, cmp) => cmp
        case PrmAddr(_)      => mainComponent
      }
  }
  private def getAddrExp(addr: Addr): Expression = addr match {
    case returnAddr: ReturnAddr[Component] @unchecked => expr(returnAddr.cmp)
    case schemeAddr: SchemeAddr[Component] @unchecked =>
      schemeAddr match {
        case VarAddr(idf, _) => idf
        case PtrAddr(exp, _) => exp
        case PrmAddr(nam)    => Identifier(nam, Identity.none)
      }
  }
  private def getAddrModule(addr: Addr): SchemeModule =
    module(getAddrCmp(addr))
  def getParentModule(clo: lat.Closure): SchemeModule =
    module(clo._2.asInstanceOf[WrappedEnv[Addr, Component]].data)
  private def sizeOfValue(value: Value): Int =
    value.vs.map(sizeOfV).sum
  private def sizeOfV(v: modularLatticeWrapper.modularLattice.Value): Int = v match {
    case modularLatticeWrapper.modularLattice.Pointer(ptrs)    => ptrs.size
    case modularLatticeWrapper.modularLattice.Clo(closures)    => closures.size
    case modularLatticeWrapper.modularLattice.Cons(car, cdr)   => sizeOfValue(car) + sizeOfValue(cdr)
    case modularLatticeWrapper.modularLattice.Vec(_, elements) => elements.map(_._2).map(sizeOfValue).sum
    case _                                                     => 0
  }
  private def printClosure(clo: lat.Closure) =
    s"${clo._1.lambdaName} (${clo._2.asInstanceOf[WrappedEnv[_, _]].data})"
}

trait AdaptiveKCFA extends AdaptiveContextSensitivity {

  type ComponentContext = List[Position]
  
  case object KUnlimited extends ContextSensitivityPolicy {
    override def toString = "k = ∞"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx
    def allocCtx(clo: lattice.Closure, 
                 args: List[Value], 
                 call: Position, 
                 caller: Component): ComponentContext = call :: getContext(caller)
  }

  case class KCallSites(k: Int) extends ContextSensitivityPolicy {
    override def toString = s"k = $k"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx.take(k)
    def allocCtx(clo: lattice.Closure, 
                 args: List[Value], 
                 call: Position, 
                 caller: Component): ComponentContext = (call :: getContext(caller)).take(k)
  }

  val defaultPolicy = KUnlimited
  def nextPolicy(clo: lattice.Closure, 
                 cur: ContextSensitivityPolicy, 
                 cts: Set[ComponentContext]): ContextSensitivityPolicy = cur match {
    case KUnlimited => 
      val highestK = cts.maxBy(_.length).length
      KCallSites(highestK - 1)
    case KCallSites(k) if k > 0 => KCallSites(k - 1)
    case _ => throw new Exception("Can not lower precision any further!")
  }
  def glbPolicy(pl1: ContextSensitivityPolicy, pl2: ContextSensitivityPolicy) = (pl1, pl2) match {
    case (KUnlimited, _) => pl2
    case (_, KUnlimited) => pl1
    case (KCallSites(k1), KCallSites(k2)) => KCallSites(Math.min(k1,k2)) 
  }

  def updateCtx(update: Component => Component)(ctx: ComponentContext) = ctx

  private def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main                                   => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
}