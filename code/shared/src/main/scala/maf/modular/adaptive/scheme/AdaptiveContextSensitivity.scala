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
    debug(s"=> ${adapted.map(printClosure)}")
    // update the summary
    summary = summary.clearDependencies(reducedDeps)
    // update the analysis
    if(adapted.nonEmpty) { updateAnalysis() }
    // clear the visited sets
    reducedCmps = Set.empty
    reducedDeps = Set.empty
    adapted = Set.empty
  }

  private def adaptBy[D](
      data: Iterable[D],
      size: D => Int,
      reduceH: => Unit,
      reduceV: D => Unit
    ): Unit =
    if (data.nonEmpty) {
      val hcount = data.size
      val vcount = size(data.maxBy(size))
      if (hcount > vcount) {
        reduceH
      } else {
        val selected = selectLargest(data, size, vcount)
        selected.foreach(reduceV)
      }
    }

  def selectLargest[D](
      data: Iterable[D],
      size: D => Int,
      max: Int
    ): Iterable[D] = {
    val target = max / 2
    data.filter(size(_) > target)
  }

  private def reduceModule(module: SchemeModule) =
    adaptBy[(Component, MultiSet[Dependency])](
      summary(module).content,
      _._2.cardinality,
      // (a) too many components for the given module
      reduceComponentsForModule(module),
      // (b) too many reanalyses of components corresponding to that module
      d => reduceReanalyses(d._2)
    )

  // look at all the components that were triggered too often
  private def reduceReanalyses(deps: MultiSet[Dependency]) =
    adaptBy[(Dependency, Int)](
      deps.content,
      _._2,
      // (a) too many dependencies triggered for a given component
      reduceAddresses(deps.toSet.map(_.asInstanceOf[AddrDependency].addr)),
      // (b) too many times triggering dependencies of that component
      d => reduceDep(d._1)
    )

  private def reduceDep(dep: Dependency) =
    if (!reducedDeps(dep)) {
      reducedDeps += dep
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

  private def reduceAddresses(addrs: Set[Addr]) =
    adaptBy[(Expression, Set[Addr])](
      addrs.groupBy(getAddrExp),
      _._2.size,
      (), // too many address locations => can't do anything about that ...
      d => reduceComponentsForModule(getAddrModule(d._2.head))
    )

  private def reduceClosures(cls: Set[lat.Closure]) =
    adaptBy[(SchemeLambdaExp, Set[lat.Closure])](
      cls.groupBy(_._1),
      _._2.size,
      (), // too many function locations => can't do anything about that ...
      d => reduceComponentsForModule(getParentModule(d._2.head))
    )

  private def reduceComponentsForModule(module: SchemeModule) = module match {
    case MainModule      => warn("Attempting to reduce the number of components for the main module")
    case l: LambdaModule => reduceComponents(l)
  }

  private def reduceComponents(module: LambdaModule): Unit =
    if (!reducedCmps(module)) {
      reducedCmps += module
      val cmps = summary(module).components
      val calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]]).toSet
      lazy val parentModule = getParentModule(calls.head.clo)
      adaptBy[(lat.Closure, Set[Call[ComponentContext]])](
        calls.groupBy(_.clo),
        _._2.size,
        // (a) too many closures
        reduceComponentsForModule(parentModule), // guaranteed to be a lambda assuming that calls.size > 1
        // (b) too many contexts per closure
        d => reduceContext(d._1, d._2)
      )
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
    kPerFn = kPerFn.foldLeft(Map.empty[lat.Closure,Int]) { case (acc,(clo,k)) =>
      val updatedClo = updateClosure(update)(clo)
      acc.get(updatedClo) match {
        case None => acc + (updatedClo -> k)
        case Some(k2) => acc + (updatedClo -> Math.min(k,k2))
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
