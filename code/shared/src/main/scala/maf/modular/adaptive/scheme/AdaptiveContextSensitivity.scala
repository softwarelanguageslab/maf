package maf.modular.adaptive.scheme

import maf.core._
import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.scheme._
import maf.modular._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.MonoidInstances

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics with SchemeModFModules {
    this: AdaptiveContextSensitivityPolicy =>

  import modularLatticeWrapper.modularLattice.{schemeLattice => lat}

  // configured by (TODO: in Scala 3, turn these into trait parameters):
  // - a threshold `n` for the number of components per module
  // - a threshold `t` for how many times a component is triggered before adaptation
  val n: Int
  val t: Int

  // disable warning messages and debug logging by default (can be override for custom logging)
  protected def warn(message: => String): Unit = ()
  protected def debug(message: => String): Unit = ()

  // use a different context-sensitivity policy per closure

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

  // data kept by the analysis, includes:
  // - the components per module
  // - the number of times a dependency is triggered
  var cmpsPerModule: Map[SchemeModule, Set[Component]] = Map.empty
  var depTriggerCounts: Map[Dependency, Int] = Map.empty

  override def spawn(cmp: Component) = {
    if (!visited(cmp)) {
      val mod = module(cmp)
      val updatedCmps = cmpsPerModule.getOrElse(mod, Set.empty) + cmp
      cmpsPerModule += mod -> updatedCmps
      if (updatedCmps.size > n) {
        markedModules += mod
      }
    }
    super.spawn(cmp)
  }

  override def trigger(dep: Dependency) = {
    val triggered = deps.getOrElse(dep, Set.empty)
    val previousCount = depTriggerCounts.getOrElse(dep, 0)
    val updatedCount = previousCount + triggered.size
    depTriggerCounts += dep -> updatedCount
    if (previousCount <= t && updatedCount > t) {
      markedDependencies += dep
    }
    super.trigger(dep)
  }

  // before adaptation, keep track of ... 
  // - which modules have too many components
  // - which dependencies are triggered too often

  private var markedModules: Set[SchemeModule] = Set.empty
  private var markedDependencies: Set[Dependency] = Set.empty 

  // ... during adaptation (to avoid duplicating work) ...
  // ... keep track of the modules for which the number of closures has been reduced

  private var reducedModules: Set[SchemeModule] = Set.empty

  // adapting the analysis
  // - for marked modules => reduce the number of components
  // - for marked components => reduce how many times the component is triggered

  def adaptAnalysis() = {
    // clear the set of reduced modules
    reducedModules = Set.empty
    // start the adaptation
    markedModules.foreach(reduceComponentsForModule)
    markedDependencies.foreach(reduceDep)
    // update the analysis
    debug(s"MARKED MODULES: $markedModules")
    debug(s"MARKED DEPENDENCIES: $markedDependencies")
    debug(s"=> REDUCED: $reducedModules")
    if(reducedModules.nonEmpty) { updateAnalysis() }
    // unmark all modules and dependencies
    markedModules = Set.empty
    markedDependencies = Set.empty
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
      val maxContextsPerClosure = callsPerClo.maxBy(_._2.size)._2.size
      // look at the number of closures vs contexts
      val numberOfClosures = callsPerClo.size
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

  private def reduceDep(dep: Dependency) = dep match {
    case AddrDependency(addr) => reduceValueAbs(store(addr))
    case _                    => throw new Exception("Unknown dependency for adaptive analysis")
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

  private def reduceClosures(cls: Set[lat.Closure]) = {
    val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
    val selected = selectLargest[(SchemeLambdaExp, Set[lat.Closure])](groupByFunction, _._2.size)
    selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }
  }

  private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[lat.Closure]) =
    reduceComponentsForModule(getParentModule(closures.head))

  // updating the analysis

  override def updateAnalysisData(update: Map[Component,Component]) = {
    super.updateAnalysisData(update)
    this.cmpsPerModule = updateMap(updateSet(update))(cmpsPerModule)
    this.depTriggerCounts = updateMap(updateDep(update), (c: Int) => c)(depTriggerCounts)(MonoidInstances.intMaxMonoid)
    this.policyPerClosure = updateMap(updateClosure(update), (p: ContextSensitivityPolicy) => p)(policyPerClosure)
  }

  /*
   * HELPERS
   * (TODO: factor our some of these ...)
   */

  def selectLargest[D](data: Iterable[D], size: D => Int): Iterable[D] =
    selectLargest(data, size, size(data.maxBy(size)))
  def selectLargest[D](
      data: Iterable[D],
      size: D => Int,
      max: Int
    ): Iterable[D] = {
    val target = Math.max(max * cutoffFactor, 1)
    data.filter(size(_) > target)
  }

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