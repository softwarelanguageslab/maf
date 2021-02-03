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

  var toAdapt: Set[SchemeModule] = Set.empty
  def onCostIncrease(module: SchemeModule, newCost: Int) =
    if (newCost > budget) {
      toAdapt += module
    }

  override protected def adaptAnalysis(): Unit = {
    super.adaptAnalysis()
    if (toAdapt.nonEmpty) {
      // adapt the components of marked modules
      // 2 possibilies:
      // (a) too many components for the given module
      // (b) too many reanalyses of the same components (of the corresponding module)
      toAdapt.foreach { module =>
        // determine the root cause of the scalability problem for function fn
        val ms = summary(module)
        val total = ms.cost
        val hcount = ms.numberOfCmps
        val vcount = Math.round(total.toFloat / hcount)
        if (hcount > vcount) {
          // (a) too many components => reduce number of components for fn
          reduceComponents(module.asInstanceOf[LambdaModule], ms) //only a lambda can have multiple components
        } else {
          // (b) too many reanalyses => reduce number of dependencies triggered for fn
          reduceDependencies(ms)
        }
        updateAnalysis()
      }
      toAdapt = Set.empty
    }
  }
  private def reduceComponents(fn: LambdaModule): Unit = reduceComponents(fn, summary.get(fn))
  private def reduceComponents(fn: LambdaModule, target: Int): Unit = reduceComponents(fn, summary.get(fn), target)
  private def reduceComponents(fn: LambdaModule, ms: ModuleSummary): Unit = reduceComponents(fn, ms, ms.numberOfCmps / 2)
  private def reduceComponents(
      module: LambdaModule,
      ms: ModuleSummary,
      target: Int
    ): Unit = {
    val cmps = ms.components
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while (calls.size > target && k > 0) { // TODO: replace with binary search?
      k = k - 1
      calls = calls.map(adaptCall(_, Some(k)))
    }
    kPerFn += module.fun -> k // register the new k
    // if lowering context does not work => adapt parent components
    // TODO: do this earlier without dropping all context first
    if (calls.size > target && target > 0) {
      val parentLambda = getParentModule(calls.head).asInstanceOf[LambdaModule] // guaranteed to have a lambda parent!
      reduceComponents(parentLambda, target)
    }
  }
  private def reduceDependencies(ms: ModuleSummary) = {
    // adapt to reduce number of triggered dependencies for a module
    // two possibilities:
    // (a) too many dependencies triggered for a given module
    // (b) too many times triggering the same dependencies
    val dependencies = ms.depCounts
    val totalCount = dependencies.cardinality
    val numberOfDeps = dependencies.distinctCount
    val averageCount = totalCount / numberOfDeps
    if (numberOfDeps > averageCount) {
      // (a) too many dependencies => reduce the number of corresponding addresses
      val addrs = dependencies.toSet.collect { case AddrDependency(addr) => addr }
      reduceAddresses(addrs)
    } else {
      // (b) too many triggers => coarsen value abstraction to avoid triggering components
      reduceValueAbs(dependencies)
    }
  }
  private def reduceAddresses(addrs: Set[Addr]) = {
    val target: Int = addrs.size / 2
    val perLocation = addrs.groupBy(_.idn) // TODO: by expr instead of idn?
    val chosenAddrs = takeLargest(perLocation.toList, (g: (_, Set[_])) => g._2.size, target)
    val chosenFuncs = chosenAddrs.map(addr => getAddrModule(addr._2.head)).collect { case l: LambdaModule =>
      l // guaranteed to be a lambda module!
    }
    // assert(chosenAddrs.size == chosenFuncs.size) // <- we expect all addresses to belong to Some(fn)
    chosenFuncs.toSet.foreach(reduceComponents)
  }
  private def reduceValueAbs(deps: MultiSet[Dependency]) = {
    val target: Int = deps.cardinality / 2
    val chosenDeps = takeLargest(deps.toList, (g: (_, Int)) => g._2, target).map(_._1)
    val chosenSourceAddrs = chosenDeps.collect { case AddrDependency(addr) => addr }
    val chosenTargetAddrs = chosenSourceAddrs.map(store).flatMap(lattice.getPointerAddresses)
    chosenDeps.foreach(dep => summary = summary.clearDependency(dep))
    reduceAddresses(chosenTargetAddrs.toSet)
  }

  /*
   * HELPERS
   */
  private def takeLargest[X](
      lst: List[X],
      size: X => Int,
      target: Int
    ): List[X] = {
    def rec(cur: List[X], todo: Int): List[X] = cur match {
      case group :: rest if todo > 0 =>
        group :: rec(rest, todo - size(group))
      case _ => Nil
    }
    rec(lst.sortBy(size)(Ordering[Int].reverse), target)
  }
  private def getAddrCmp(addr: Addr): Component = addr match {
    case returnAddr: ReturnAddr[Component] @unchecked => returnAddr.cmp
    case schemeAddr: SchemeAddr[Component] @unchecked =>
      schemeAddr match {
        case VarAddr(_, ctx) => ctx
        case PtrAddr(_, ctx) => ctx
        case _               => mainComponent
      }
    case _ => mainComponent
  }
  private def getAddrModule(addr: Addr): SchemeModule =
    module(getAddrCmp(addr))
  private def getParentModule(call: Call[ComponentContext]): SchemeModule =
    module(call.clo._2.asInstanceOf[WrappedEnv[Addr, Component]].data)
}
