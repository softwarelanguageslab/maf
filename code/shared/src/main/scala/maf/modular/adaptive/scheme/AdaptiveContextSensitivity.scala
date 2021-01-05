package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.modular.scheme.modf._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.util.Monoid
import maf.util.MonoidImplicits._
import maf.util.benchmarks.Timeout
import maf.core._
import maf.modular.scheme._
import maf.modular._

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics {
  /*
   * configured by some "budget" (which determines how quickly a function will adapt its components)
   */
  val budget: Int
  /*
   * keep track of:
   * - the number of times a function has been analysed
   * - through which dependencies the analysis of a function was triggered
   * - which components correspond to a given function
   */
  var counts:       Map[SchemeExp, Int]                   = Map.empty
  var triggeredBy:  Map[SchemeExp, Map[Dependency, Int]]  = Map.empty
  var triggered:    Map[Dependency, Set[SchemeExp]]       = Map.empty
  var cmpsPerFn:    Map[SchemeExp, Set[Component]]        = Map.empty
  private def incCount(cmp: Component): Unit = incCount(expr(cmp))
  private def incCount(fn: SchemeExp): Unit = {
    val updated = counts.getOrElse(fn, 0) + 1
    counts += fn -> updated
    if (updated > budget) {
      toAdapt += fn
    } 
  }
  override def trigger(dep: Dependency): Unit = {
    deps.getOrElse(dep, Set.empty).foreach { cmp => 
      val fn = expr(cmp)
      // increase the count of the corresponding function
      incCount(fn)
      // bookkeeping for the  dependency
      val previous = triggeredBy.getOrElse(fn, Map.empty)
      val updated = previous + (dep -> (previous.getOrElse(dep, 0) + 1))
      triggeredBy += fn -> updated
      triggered += dep -> (triggered.getOrElse(dep, Set.empty) + fn)
    }
    // trigger the dependency
    super.trigger(dep)
  }
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    // increase the count
    incCount(cmp)
    // bookkeeping: register every new component with the corresponding lambda
    val lambda = call.clo._1
    val lambdaCmps = cmpsPerFn.getOrElse(lambda, Set.empty)
    val lambdaCmpsUpdated = lambdaCmps + cmp
    cmpsPerFn += lambda -> lambdaCmpsUpdated
  }
  // correctly updating the analysis data
  private implicit val intMaxMonoid: Monoid[Int] = new Monoid[Int] { 
    def zero: Int = 0 // assumption: only safe for nonnegative integers
    def append(x: Int, y: => Int): Int = Math.max(x,y)
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
    this.triggered = updateMap(updateDep(update), (s:Set[SchemeExp]) => s)(triggered)
    this.triggeredBy = updateMap(updateMap(updateDep(update), (c:Int) => c))(triggeredBy)
  }
  /*
   * contexts are call-site strings
   * after adaptation, certain strings get trimmed
   */
  type ComponentContext = List[Position]
  var kPerFn = Map[SchemeExp, Int]()
  def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    adaptCall(cll, kPerFn.get(cll.clo._1))
  def adaptCall(cll: Call[ComponentContext], kLimit: Option[Int]): Call[ComponentContext] =
    cll.copy(ctx = adaptCtx(cll.ctx, kLimit))
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext =
    adaptCtx(call :: getContext(caller), kPerFn.get(clo._1))
  def adaptCtx(ctx: ComponentContext, kLimit: Option[Int]): ComponentContext =
    kLimit match {
      case None     => ctx    
      case Some(k)  => ctx.take(k)
    }
  def updateCtx(update: Component => Component)(ctx: ComponentContext): ComponentContext = ctx
  /**
    * Adapting the analysis
    */
  var toAdapt: Set[SchemeExp] = Set.empty
  override protected def adaptAnalysis(): Unit = {
    super.adaptAnalysis()
    if(toAdapt.nonEmpty) {
      // adapt the components of marked functions
      // 2 possibilies:
      // - too many components for the given function
      // - too many reanalyses of the same components (of the corresponding function)
      toAdapt.foreach { fn =>
        // determine the root cause of the scalability problem for function fn
        val total = counts(fn)
        val cmps = cmpsPerFn(fn)
        val hcount = cmps.size
        val vcount = Math.round(total.toFloat / hcount) 
        if (hcount > vcount) {                  // (a) too many components ...
          reduceComponents(fn, cmps)            // => reduce number of components for fn
        } else {                                // (b) too many reanalyses ...
          reduceDependencies(fn)                // => reduce number of dependencies triggered for fn
        }
        // reset budget and bookkeeping for the adapted function
        counts += fn -> 0
        // update the analysis data structures
        updateAnalysis()
      }
      toAdapt = Set.empty
    }
  }
  private def reduceComponents(fn: SchemeExp): Unit = reduceComponents(fn, cmpsPerFn(fn))
  private def reduceComponents(fn: SchemeExp, cmps: Set[Component]): Unit = reduceComponents(fn, cmps, cmps.size/2)
  private def reduceComponents(fn: SchemeExp, cmps: Set[Component], target: Int): Unit = {
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while(calls.size > target && k > 0) { // TODO: replace with binary search?
      k = k - 1
      calls = calls.map(adaptCall(_, Some(k)))
    } 
    //println(s"$fn -> $k")
    kPerFn += fn -> k  // register the new k
    // if lowering context does not work => adapt parent components
    // TODO: do this earlier without dropping all context first
    if(calls.size > target) {
      val parentCmps = calls.map(cll => cll.clo._2.asInstanceOf[WrappedEnv[Addr,Component]].data)
      val parentLambda = view(parentCmps.head).asInstanceOf[Call[ComponentContext]].clo._1
      reduceComponents(parentLambda, parentCmps, target)
    }
  }
  private def reduceDependencies(fn: SchemeExp): Unit = {
    val dependencies = triggeredBy(fn)
    val numberOfDeps = dependencies.size
    val averageCount = dependencies.values.sum / numberOfDeps
    if (numberOfDeps > averageCount) {
      val addrs = dependencies.keySet.collect {
        case AddrDependency(addr) => addr
      }
      reduceAddresses(addrs)
    } else {
      adaptByCoarsen(fn, dependencies)
    }
  }
  private def takeLargestGroups[X,Y](grouped: Map[X,Set[Y]], minimum: Int): List[(X,Set[Y])] = {
    def rec(cur: List[(X,Set[Y])], todo: Int):  List[(X,Set[Y])] = cur match {
      case (next@(key, set)) :: rest if todo > 0 => 
        next :: rec(rest, todo - set.size)
      case _ => Nil
    }
    val groupList = grouped.toList
    val sortedAsc = groupList.sortBy(_._2.size)
    val sortedDsc = sortedAsc.reverse // TODO: just reverse the ordering
    rec(sortedDsc, minimum)
  }
  private def getAddrCmp(addr: Addr): Option[Component] = addr match {
    case returnAddr: ReturnAddr[Component] => Some(returnAddr.cmp)
    case schemeAddr: SchemeAddr[Component] @unchecked => schemeAddr match {
      case VarAddr(_,ctx) => Some(ctx)
      case PtrAddr(_,ctx) => Some(ctx)
      case _ => None
    }
    case _ => None 
  }
  private def getAddrFn(addr: Addr): Option[SchemeExp] = 
    getAddrCmp(addr).map(view).flatMap {
      case Call((fnExp,_),_,_) => Some(fnExp) 
      case _ => None
    }
  private def reduceAddressesFns(addrs: Set[Addr]): Set[SchemeExp] = {
    val target: Int = addrs.size / 2
    val perLocation = addrs.groupBy(_.idn)
    val chosenAddrs = takeLargestGroups(perLocation, target)
    val chosenFuncs = chosenAddrs.map(_._2.head).flatMap(getAddrFn)
    // assert(chosenAddrs.size == chosenFuncs.size) // <- we expect all addresses to belong to Some(fn)
    return chosenFuncs.toSet
  }
  private def reduceAddresses(addrs: Set[Addr]): Unit = reduceAddressesFns(addrs).foreach(reduceComponents)
  private def adaptByCoarsen(fn: SchemeExp, dependencies: Map[Dependency, Int]): Unit = {
    ???
  }
}
