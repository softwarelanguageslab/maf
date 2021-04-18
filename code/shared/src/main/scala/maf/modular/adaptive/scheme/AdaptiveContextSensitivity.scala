package maf.modular.adaptive.scheme

import maf.core._
import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.scheme._
import maf.modular._
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.datastructures.MultiSet
import scala.annotation.tailrec

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics with SchemeModFModules {
  this: AdaptiveContextSensitivityPolicy =>

  import modularLatticeWrapper.modularLattice.{schemeLattice => lat}

  // configured by (TODO: in Scala 3, turn these into trait parameters):
  // - a threshold `n` for the number of components per module
  // - a threshold `t` for how many components are re-analysed due to triggering dependencies at a single location
  val n: Int
  val t: Int

  // disable warning messages and debug logging by default (can be override for custom logging)
  protected def warn(message: => String): Unit = ()
  protected def debug(message: => String): Unit = ()

  // map all lambdas to identities

  override def program = {
    val prg = super.program
    collectFnIdentities(prg)
    prg
  }

  private var fnIdentities: Map[Identity, SchemeLambdaExp] = Map.empty

  private def collectFnIdentities(exp: Expression): Unit = exp match {
    case lam: SchemeLambdaExp =>
      fnIdentities += lam.idn -> lam
      lam.subexpressions.foreach(collectFnIdentities)
    case _ => 
      exp.subexpressions.foreach(collectFnIdentities)
  }

  // use a different context-sensitivity policy per closure

  private var policyPerFn: Map[Identity, ContextSensitivityPolicy] = Map.empty
  private def getCurrentPolicy(fnIdn: Identity): ContextSensitivityPolicy =
    policyPerFn.getOrElse(fnIdn, defaultPolicy)
  private def getCurrentPolicy(fn: SchemeLambdaExp): ContextSensitivityPolicy =
    getCurrentPolicy(fn.idn)
  private def setCurrentPolicy(fnIdn: Identity, ply: ContextSensitivityPolicy): Unit =
    policyPerFn += fnIdn -> ply
  private def setCurrentPolicy(fn: SchemeLambdaExp, ply: ContextSensitivityPolicy): Unit =
    setCurrentPolicy(fn.idn, ply)

  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    getCurrentPolicy(clo._1).allocCtx(clo, args, call, caller)
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    cll.copy(ctx = getCurrentPolicy(cll.clo._1).adaptCtx(cll.ctx))

  // allocation context = component context
  // (also store the function that the context came from)

  type AllocationContext = Option[(ComponentContext, Identity)]
  def adaptAllocCtx(ctx: AllocationContext): AllocationContext = ctx.map {
    case (ctx, idn) => (getCurrentPolicy(idn).adaptCtx(ctx), idn)
  }
  private def addrContext(cmp: SchemeModFComponent) = cmp match {
    case Main => None
    case Call((lam, _), ctx: ComponentContext @unchecked) => Some((ctx, lam.idn))
  }
  def allocPtr(exp: SchemeExp, cmp: SchemeModFComponent) = PtrAddr(exp, addrContext(cmp))
  def allocVar(idf: Identifier, cmp: SchemeModFComponent) = VarAddr(idf, addrContext(cmp))

  // data kept by the analysis, includes:
  // - the components per module
  // - the number of times dependencies are triggered
  private var cmpsPerModule: Map[SchemeModule, Set[Component]] = Map.empty
  private var triggerCounts: Map[Expression, MultiSet[Dependency]] = Map.empty

  override def spawn(cmp: Component) = {
    if (!visited(cmp)) {
      val mod = module(cmp)
      val previousCmps = cmpsPerModule.getOrElse(mod, Set.empty)
      val updatedCmps = previousCmps + cmp
      cmpsPerModule += mod -> updatedCmps
      if (updatedCmps.size > n) {
        markedModules += mod
      }
    }
    super.spawn(cmp)
  }

  override def trigger(dep: Dependency) = {
    val location = getDepExp(dep)
    val triggered = deps.getOrElse(dep, Set.empty).size
    val previousCounts = triggerCounts.getOrElse(location, MultiSet.empty)
    val updatedCounts = previousCounts.updateMult(dep)(_ + triggered)
    triggerCounts += location -> updatedCounts
    if (updatedCounts.cardinality > t) {
      markedLocations += location
    }
    super.trigger(dep)
  }

  // before adaptation, keep track of ...
  // - which modules have too many components
  // - which locations are triggered too often

  private var markedModules: Set[SchemeModule] = Set.empty
  private var markedLocations: Set[Expression] = Set.empty

  // ... during adaptation (to avoid duplicating work) ...
  // ... keep track of the modules for which the number of closures has been reduced

  private var reducedModules: Set[SchemeModule] = Set.empty

  // adapting the analysis
  // - for marked modules => reduce the number of components
  // - for marked components => reduce how many times the component is triggered

  def inspect() = {
    // clear the set of reduced modules
    reducedModules = Set.empty
    // start the adaptation
    markedModules.foreach(reduceComponentsForModule)
    markedLocations.foreach(reduceTriggersForLocation)
    // update the analysis
    debug(s"MARKED MODULES: $markedModules")
    debug(s"MARKED LOCATIONS: ${markedLocations.map(_.idn)}")
    debug(s"=> REDUCED: $reducedModules")
    if (reducedModules.nonEmpty) { adaptAnalysis() }
    // unmark all modules and dependencies
    markedModules = Set.empty
    markedLocations = Set.empty
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
      val calls = cmpsPerModule(module).map(_.asInstanceOf[Call[ComponentContext]])
      val contexts = calls.map(_.ctx)
      if (contexts.size > 1) {
        val target = Math.max(1, reduceFactor * contexts.size).toInt
        val policy = getCurrentPolicy(module.lambda)
        reduceContext(module, policy, contexts, target)
      } else {
        reduceComponentsForModule(getParentModule(calls.head.clo))
      }
    }

  // find a fitting policy
  @tailrec
  private def reduceContext(module: LambdaModule, 
                            policy: ContextSensitivityPolicy, 
                            ctxs: Set[ComponentContext], 
                            target: Int): Unit = {
    if (ctxs.size > target) {
      // need to decrease precision further
      val next = nextPolicy(module.lambda, policy, ctxs)
      val adapted = ctxs.map(next.adaptCtx)
      reduceContext(module, next, adapted, target)
    } else {
      // register the new policy
      debug(s"$module -> $policy")
      setCurrentPolicy(module.lambda, policy)
    }
  }

  private def reduceTriggersForLocation(loc: Expression) = {
    val depCounts = triggerCounts.getOrElse(loc, MultiSet.empty)
    val selected = selectLargest[(Dependency,Int)](depCounts.content, _._2)
    val updated = selected.foldLeft(depCounts) { case (acc, (dep, _)) => 
      reduceDep(dep) 
      acc - dep
    }
    triggerCounts += loc -> updated
  }

  private def reduceDep(dep: Dependency) = dep match {
    case AddrDependency(addr) => reduceValueAbs(store(addr))
    case _ => throw new Exception("Unknown dependency for adaptive analysis")
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
    case v => warn(s"Attempting to adapt a non-set-based value $v")
  }

  private def reduceAddresses(addrs: Set[Addr]) = {
    val groupByLocation = addrs.groupBy[Expression](getAddrExp)
    val selected = selectLargest[(Expression, Set[Addr])](groupByLocation, _._2.size)
    selected.foreach { case (loc, addrs) => reduceAddressesForLocation(loc, addrs) }
  }

  private def reduceAddressesForLocation(loc: Expression, addrs: Set[Addr]) = {
    debug(s"Reducing ${addrs.size} addrs")
    reduceComponentsForModule(getAddrModule(addrs.head))
  }

  private def reduceClosures(cls: Set[lat.Closure]) = {
    val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
    val selected = selectLargest[(SchemeLambdaExp, Set[lat.Closure])](groupByFunction, _._2.size)
    selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }
  }

  private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[lat.Closure]) = {
    debug(s"Reducing ${closures.size} closures")
    reduceComponentsForModule(getParentModule(closures.head))
  }

  // adapting the analysis

  override def adaptAnalysis() = {
    super.adaptAnalysis()
    this.cmpsPerModule = adaptMap(adaptSet(adaptComponent))(cmpsPerModule)
    this.triggerCounts = adaptMap(adaptMultiSet(adaptDep, Math.max))(triggerCounts)
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
 
  private def getDepExp(dep: Dependency): Expression = dep match {
    case AddrDependency(addr) => getAddrExp(addr)
    case _ => throw new Exception(s"Unknown dependency: $dep")
  }
  private def getAddrExp(addr: Addr): Expression = addr match {
    case returnAddr: ReturnAddr[Component] @unchecked => expr(returnAddr.cmp)
    case schemeAddr: SchemeAddr[AllocationContext] @unchecked =>
      schemeAddr match {
        case VarAddr(idf, _) => idf
        case PtrAddr(exp, _) => exp
        case PrmAddr(nam)    => Identifier(nam, Identity.none)
      }
  }
  private def getAddrModule(addr: Addr): SchemeModule = addr match {
    case returnAddr: ReturnAddr[Component] @unchecked => module(returnAddr.cmp)
    case schemeAddr: SchemeAddr[AllocationContext] @unchecked =>
      schemeAddr match {
        case VarAddr(_, ctx) => getAllocCtxModule(ctx)
        case PtrAddr(_, ctx) => getAllocCtxModule(ctx)
        case PrmAddr(_)      => MainModule
      }
  }
  private def getAllocCtxModule(ctx: AllocationContext): SchemeModule = ctx match {
    case None => MainModule
    case Some((_, fnIdn)) => LambdaModule(fnIdentities(fnIdn))
  }
  lazy val mainIdn = mainBody.idn
  def getParentModule(clo: lat.Closure): SchemeModule = {
    val parentIdn = clo._2.asInstanceOf[WrappedEnv[Addr, Identity]].data
    if (parentIdn == mainIdn) {
      MainModule 
    } else {
      LambdaModule(fnIdentities(parentIdn))
    }
  }
  private def sizeOfValue(value: Value): Int =
    value.vs.map(sizeOfV).sum
  private def sizeOfV(v: modularLatticeWrapper.modularLattice.Value): Int = v match {
    case modularLatticeWrapper.modularLattice.Pointer(ptrs)    => ptrs.size
    case modularLatticeWrapper.modularLattice.Clo(closures)    => closures.size
    case modularLatticeWrapper.modularLattice.Cons(car, cdr)   => sizeOfValue(car) + sizeOfValue(cdr)
    case modularLatticeWrapper.modularLattice.Vec(_, elements) => elements.map(_._2).map(sizeOfValue).sum
    case _                                                     => 0
  }
}
