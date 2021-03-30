package maf.modular.adaptive.scheme

import maf.core._
import maf.language.scheme._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.util.datastructures._
import maf.modular.scheme._
import maf.modular._
import maf.modular.scheme.modf.SchemeModFComponent._

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics with AdaptiveAnalysisSummary {

  import modularLatticeWrapper.modularLattice.{schemeLattice => lat}

  protected def warn(message: => String): Unit = println(message)
  protected def debug(message: => String): Unit = println(message)

  /*
   * configured by:
   * - some "budget" (which, when exceeded by the "cost" of some function, triggers an adaptation)
   *  => this parameter determines how quickly we trigger an adaptation
   */
  val budget: Int

  def modulesToAdapt = summary.content.collect {
    case (module, moduleSummary) if moduleSummary.cost > budget => module
  }

  /*
   * contexts are call-site strings
   * after adaptation, certain strings get trimmed
   */
  type ComponentContext = List[Position]
  var kPerFn = Map[lat.Closure, Int]()
  def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main                                   => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    cll.copy(ctx = adaptContext(cll.clo, cll.ctx))
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    adaptContext(clo, call :: getContext(caller))
  def adaptContext(clo: lat.Closure, ctx: ComponentContext): ComponentContext =
    kPerFn.get(clo) match {
      case None    => ctx
      case Some(k) => ctx.take(k)
    }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext = ctx

  /** Adapting the analysis */

  private var reducedCmps: Set[LambdaModule] = Set.empty
  private var reducedDeps: Set[Dependency] = Set.empty

  // for debugging purposes only
  private var adapted: Set[lat.Closure] = Set.empty

  def adaptAnalysis() = {
    // start the adaptation
    val modules = modulesToAdapt
    modules.foreach(reduceModule)
    debug(s"${modules.mkString("[", ",", "]")} => ${adapted.map(printClosure).mkString("[", ",", "]")}")
    // update the summary
    summary = summary.clearDependencies(reducedDeps)
    // update the analysis
    if (adapted.nonEmpty) { updateAnalysis() }
    // clear the visited sets
    reducedCmps = Set.empty
    reducedDeps = Set.empty
    adapted = Set.empty
  }

  def selectLargest[D](data: Iterable[D], size: D => Int): Iterable[D] =
    selectLargest(data, size, size(data.maxBy(size)))
  def selectLargest[D](
      data: Iterable[D],
      size: D => Int,
      max: Int
    ): Iterable[D] = {
    val target = max / 2
    data.filter(size(_) > target)
  }

  private def reduceModule(module: SchemeModule) = {
    val moduleSummary = summary(module)
    val numberOfComponents = moduleSummary.numberOfCmps
    val maximumComponentCost = moduleSummary.maxComponentCost
    if (numberOfComponents > maximumComponentCost) {
      reduceComponentsForModule(module)
    } else {
      val selectedCmps = selectLargest[(Component, MultiSet[Dependency])](moduleSummary.content, _._2.cardinality, maximumComponentCost)
      selectedCmps.foreach { case (_, deps) => reduceDependencies(deps) }
    }
  }

  // look at all the components that were triggered too often
  private def reduceDependencies(deps: MultiSet[Dependency]) = {
    val groupByLocation = deps.groupBy[Expression] { case AddrDependency(addr) =>
      getAddrExp(addr)
    }
    val selected = selectLargest[(Expression, MultiSet[Dependency])](groupByLocation, _._2.cardinality)
    selected.foreach { case (loc, deps) => reduceDependenciesForLocation(loc, deps) }
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
    if (!reducedDeps(dep)) {
      reducedDeps += dep
      dep match {
        case AddrDependency(addr) => println(store(addr)); reduceValueAbs(store(addr))
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

  private def reduceClosures(cls: Set[lat.Closure]) = {
    val groupByFunction = cls.groupBy[SchemeLambdaExp](_._1)
    val selected = selectLargest[(SchemeLambdaExp, Set[lat.Closure])](groupByFunction, _._2.size)
    selected.foreach { case (fn, closures) => reduceClosuresForFunction(fn, closures) }
  }

  private def reduceClosuresForFunction(fn: SchemeLambdaExp, closures: Set[lat.Closure]) =
    reduceComponentsForModule(getParentModule(closures.head))

  private def reduceComponentsForModule(module: SchemeModule) = module match {
    case MainModule      => warn("Attempting to reduce the number of components for the main module")
    case l: LambdaModule => reduceComponents(l)
  }

  private def reduceComponents(module: LambdaModule): Unit =
    if (!reducedCmps(module)) {
      reducedCmps += module
      val cmps = summary(module).components
      val calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]]).toSet
      // look at the number of closures vs contexts
      val groupedByClo = calls.groupBy[lat.Closure](_.clo)
      val numberOfClosures = groupedByClo.size
      val maxContextsPerClosure = groupedByClo.maxBy(_._2.size)._2.size
      if (numberOfClosures > maxContextsPerClosure) {
        val fn = calls.head.clo._1
        reduceClosuresForFunction(fn, groupedByClo.keySet)
      } else {
        val selected = selectLargest[(lat.Closure, Set[Call[ComponentContext]])](groupedByClo, _._2.size, maxContextsPerClosure)
        selected.foreach { case (clo, calls) => reduceContext(clo, calls) }
      }
    }

  private def reduceContext(closure: lat.Closure, calls: Set[Call[ComponentContext]]): Unit = {
    // find a fitting k
    if (calls.size == 1) {
      warn(s"Attempting to reduce contexts for a single call ${calls.head}")
      return
    }
    val target = calls.size / 2
    var contexts = calls.map(_.ctx)
    var k = contexts.maxBy(_.length).length
    while (contexts.size > target) { // TODO: replace with binary search?
      k = k - 1
      contexts = contexts.map(_.take(k))
    }
    adapted += closure
    debug(s"${printClosure(closure)} -> $k")
    kPerFn += closure -> k // register the new k
  }

  private def printClosure(clo: lat.Closure) =
    s"${clo._1.lambdaName} (${clo._2.asInstanceOf[WrappedEnv[_, _]].data})"

  /*
   * Updating the analysis data
   */
  override def updateAnalysisData(update: Map[Component, Component]): Unit = {
    super.updateAnalysisData(update)
    kPerFn = kPerFn.foldLeft(Map.empty[lat.Closure, Int]) { case (acc, (clo, k)) =>
      val updatedClo = updateClosure(update)(clo)
      acc.get(updatedClo) match {
        case None     => acc + (updatedClo -> k)
        case Some(k2) => acc + (updatedClo -> Math.min(k, k2))
      }
    }
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
}
