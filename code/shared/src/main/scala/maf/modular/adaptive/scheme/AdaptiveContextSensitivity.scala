package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.modular.scheme.modf._
import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.Dependency
import maf.util.benchmarks.Timeout

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
  var counts: Map[SchemeExp, Int] = Map.empty
  var triggeredBy: Map[SchemeExp, Set[Dependency]] = Map.empty
  var cmpsPerFn: Map[SchemeLambdaExp, Set[Component]] = Map.empty
  private def incCount(cmp: Component): Unit = incCount(expr(cmp))
  private def incCount(fn: SchemeExp): Unit = {
    val updated = counts.getOrElse(fn, 0) + 1
    counts += fn -> updated
    if (updated > budget) {
      toAdapt += fn
    } 
  }
  override def trigger(dep: Dependency): Unit = {
    // increase the count and register the triggered dependency
    deps.getOrElse(dep, Set.empty).foreach { cmp => 
      val fn = expr(cmp)
      incCount(fn)
      triggeredBy += fn -> (triggeredBy.getOrElse(fn, Set.empty) + dep)
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
  /*
   * contexts are call-site strings
   * after adaptation, certain strings get trimmed
   */
  type ComponentContext = List[Position]
  var kPerFn = Map[SchemeLambdaExp, Int]()
  def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext =
    adaptCtx(clo._1, call :: getContext(caller))
  def adaptCall(cll: Call[ComponentContext]): Call[ComponentContext] =
    cll.copy(ctx = adaptCtx(cll.clo._1, cll.ctx))
  def adaptCtx(lambda: SchemeLambdaExp, ctx: ComponentContext): ComponentContext =
    kPerFn.get(lambda) match {
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
    while(visited.size > Math.max(budget, cmpsPerFn.size)) {
      val (lambda, cmps) = cmpsPerFn.maxBy(_._2.size)
      val target = (cmpsPerFn - lambda).maxByOption(_._2.size) match {
        case None                 => budget
        case Some((_, nextCmps))  => Math.max(1, nextCmps.size - 1)
      }
      adaptFunction(lambda, cmps, target)
      updateAnalysis()
    }
  }
  private def adaptFunction(lambda: SchemeLambdaExp, cmps: Set[Component], target: Int): Unit = {
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while(calls.size > target && k > 0) { // TODO: replace with binary search?
      k = k - 1
      calls = calls.map(cll => cll.copy(ctx = cll.ctx.take(k)))
    } 
    //println(s"$lambda -> $k")
    kPerFn += lambda -> k  // register the new k
    // if lowering context does not work => adapt parent components
    if(calls.size > target) {
      val parentCmps = calls.map(cll => cll.clo._2.asInstanceOf[WrappedEnv[Addr,Component]].data)
      val parentLambda = view(parentCmps.head).asInstanceOf[Call[ComponentContext]].clo._1
      adaptFunction(parentLambda, parentCmps, target)
    }
  }
  /**
    * Updating the analysis data
    */
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
  }
}
