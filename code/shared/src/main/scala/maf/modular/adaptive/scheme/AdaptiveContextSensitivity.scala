package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.modular.scheme.modf._
import maf.modular.adaptive.scheme._
import maf.core.Position._

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics {
  /*
   * configured by some "budget" (= max number of components in the analysis) 
   */
  val budget: Int
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
  /*
   * keep track of components per function
   */
  var cmpsPerFn = Map[SchemeLambdaExp, Set[Component]]()
  var toAdapt = Set[(SchemeLambdaExp,Int)]()
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    // bookkeeping: register every new component with the corresponding lambda
    val lambda = call.clo._1
    val lambdaCmps = cmpsPerFn.getOrElse(lambda, Set.empty)
    val lambdaCmpsUpdated = lambdaCmps + cmp
    cmpsPerFn += lambda -> lambdaCmpsUpdated
    // if more components than allowed: adapt the analysis
    if(lambdaCmpsUpdated.size > budget) {
      val depth = call.clo._2.asInstanceOf[WrappedEnv[Addr, Component]].depth
      toAdapt += (lambda -> depth)
    }
  }
  override protected def adaptAnalysis(): Unit = {
    toAdapt = Set.empty
    super.adaptAnalysis()
    toAdapt.toList.sortBy((_._2)).foreach { case (lambda, _) =>
      val cmps = cmpsPerFn.getOrElse(lambda, Set.empty)
      adaptFunction(lambda, cmps)
      updateAnalysis()
    }
  }
  private def adaptFunction(lambda: SchemeLambdaExp, cmps: Set[Component]): Unit = {
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while(calls.size > budget) { // TODO: replace with binary search?
      //assert(k > 0)
      k = Math.floor(k / 2).toInt
      calls = calls.map(cll => cll.copy(ctx = cll.ctx.take(k)))
    } 
    //println(s"$lambda -> $k")
    kPerFn += lambda -> k  // register the new k
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
  }
}
