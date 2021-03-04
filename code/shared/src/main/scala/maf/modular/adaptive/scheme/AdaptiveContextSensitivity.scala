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
  var kPerFn = Map[SchemeLambdaExp, Int]()
  def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main                                   => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    adaptCall(cll, kPerFn.get(cll.clo._1))
  def adaptCall(cll: Call[ComponentContext], kLimit: Option[Int]): Call[ComponentContext] =
    cll.copy(ctx = adaptCtx(cll.ctx, kLimit))
  def allocCtx(
      nam: Option[String],
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    adaptCtx(call :: getContext(caller), kPerFn.get(clo._1))
  def adaptCtx(ctx: ComponentContext, kLimit: Option[Int]): ComponentContext =
    kLimit match {
      case None    => ctx
      case Some(k) => ctx.take(k)
    }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext = ctx

  /** Adapting the analysis */

  private var reducedCmps: Set[LambdaModule] = Set.empty
  private var reducedDeps: Set[Dependency] = Set.empty

  def adaptAnalysis() = {
    // start the adaptation
    val modules = modulesToAdapt
    modules.foreach(reduceModule)
    // update the summary
    summary = summary.clearDependencies(reducedDeps)
    // update the analysis
    updateAnalysis()
    // clear the visited sets
    reducedCmps = Set.empty
    reducedDeps = Set.empty
  }

  // look at a module to adapt
  def reduceModule(module: SchemeModule) = {
    // 2 possibilies:
    // (a) too many components for the given module
    // (b) too many reanalyses of components corresponding to that module
    val ms = summary(module)
    val hcount = ms.numberOfCmps
    val vcount = ms.cost / hcount
    if (hcount > vcount) {
      // (a) too many components => reduce number of components for fn
      // note: only a lambda (i.e., not the main component) can have multiple components
      reduceComponents(module.asInstanceOf[LambdaModule])
    } else {
      // (b) too many reanalyses => reduce number of dependencies triggered for the components
      // CURRNET: per component
      val target = ms.totalDepCount / 2
      val chosen = takeLargest(ms.content, (p: (Component, MultiSet[Dependency])) => p._2.cardinality, target)
      chosen.foreach({ case (_, dps) => reduceReanalyses(dps) })
      // ALTERNATIVE CODE: aggregated for all components of that module
      //reduceReanalyses(ms.depCounts)
    }
  }

  // look at all the components that were triggered too often
  private def reduceReanalyses(deps: MultiSet[Dependency]) = {
    // 2 possibilities:
    // (a) too many dependencies triggered for a given component
    // (b) too many times triggering dependencies of that component
    val hcount = deps.distinctCount
    val vcount = deps.cardinality / hcount
    if (hcount > vcount) { // TODO: is this branch ever taken?
      // (a) too many dependencies => reduce the number of corresponding addresses
      throw new Exception("This branch should never be taken?")
      val addrs = deps.toSet.collect { case AddrDependency(addr) => addr }
      reduceAddresses(addrs)
    } else {
      // (b) too many triggers => coarsen value abstraction to avoid triggering components
      val target = deps.cardinality / 2
      val chosen = takeLargest(deps.content, (p: (Dependency, Int)) => p._2, target)
      chosen.foreach { case (dep, _) => reduceDep(dep) }
    }
  }

  private def reduceDep(dep: Dependency) =
    if (!reducedDeps(dep)) {
      reducedDeps += dep
      dep match {
        case AddrDependency(addr) => reduceValueAbs(store(addr))
        case _ => throw new Exception("Unknown dependency for adaptive analysis")
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
    case v => throw new Exception(s"Unsupported value in reduceValueAbs: $v")
  }

  private def reduceAddresses(addrs: Set[Addr]) = {
    val target: Int = addrs.size / 2
    val perLocation = addrs.groupBy(getAddrExp) // TODO: by expr instead of idn?
    val chosenAddrs = takeLargest[(Expression, Set[Addr])](perLocation.toList, _._2.size, target)
    val chosenFuncs =
      chosenAddrs.map(_._2).filter(_.size > 1).map(addrs => getAddrModule(addrs.head).asInstanceOf[LambdaModule]) // guaranteed to be a lambda module!
    chosenFuncs.toSet.foreach(reduceComponents)
  }

  private def reduceClosures(cls: Set[ClosureWithName]) = {
    val target: Int = cls.size / 2
    val perFunction = cls.groupMapReduce(module)(_ => 1)(_ + _)
    val chosenFuncs = takeLargest[(LambdaModule, Int)](perFunction, _._2, target).map(_._1)
    chosenFuncs.toSet.foreach(reduceComponents)
  }

  private def reduceComponents(module: LambdaModule): Unit =
    if (!reducedCmps(module)) {
      reducedCmps += module
      // 2 possibilities:
      // (a) too many closures
      // (b) too many contexts per closure
      val ms = summary(module)
      val closures = ms.components.map(view).collect { case c: Call[_] => c.clo }
      val hcount = closures.size
      val vcount = ms.numberOfCmps / hcount
      if (hcount > vcount) {
        // (a) too many closures => reduce the number of components for the parent module
        val parentModule = getParentModule(closures.head).asInstanceOf[LambdaModule] // guaranteed to be a lambda assuming that closures.size > 1
        reduceComponents(parentModule)
      } else {
        // (b) too many contexts per closure => reduce context information
        reduceContext(module, ms)
      }
    }

  private def reduceContext(module: LambdaModule, ms: ModuleSummary) = {
    val cmps = ms.components
    val target = ms.numberOfCmps / 2
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while (calls.size > target && k > 0) { // TODO: replace with binary search?
      k = k - 1
      calls = calls.map(adaptCall(_, Some(k)))
    }
    //println(s"${module.fun} -> $k")
    kPerFn += module.fun -> k // register the new k
  }

  /*
   * HELPERS
   */
  private def takeLargest[X](
      elms: Iterable[X],
      size: X => Int,
      target: Int
    ): List[X] = {
    val pq = scala.collection.mutable.PriorityQueue.from(elms)(Ordering.by(size))
    def rec(todo: Int): List[X] =
      if (todo > 0 && pq.nonEmpty) {
        val next = pq.dequeue()
        next :: rec(todo - size(next))
      } else {
        Nil
      }
    rec(target)
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
  private def getParentModule(clo: Closure): SchemeModule =
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
