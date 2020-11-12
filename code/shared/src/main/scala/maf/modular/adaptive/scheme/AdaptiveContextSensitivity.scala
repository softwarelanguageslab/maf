package maf.modular.adaptive.scheme

import maf.language.scheme._
import maf.util.MonoidImplicits._
import maf.modular.scheme.modf._
import maf.modular.adaptive.scheme._
import maf.util.Monoid
import maf.util.MonoidImplicits
import maf.modular.scheme._
import maf.modular._
import maf.core._
import maf.core.Position._
import maf.modular.components.IndirectComponents

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
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    val lambda = call.clo._1
    val lambdaCmps = cmpsPerFn.getOrElse(lambda, Set.empty)
    val lambdaCmpsUpdated = lambdaCmps + cmp
    cmpsPerFn += lambda -> lambdaCmpsUpdated
  }
  override protected def adaptAnalysis(): Unit = {
    super.adaptAnalysis()
    // check if we need to adapt
    if(visited.size > budget) {
      val (lambda, cmps) = cmpsPerFn.maxBy(_._2.size) // TODO: optimise this operation
      if(cmps.size == 1) { throw new Exception("Budget could not be satified") } // TODO: just increase the budget?
      val target = Math.max(1, cmps.size - (visited.size - budget))
      adaptFunction(lambda, cmps, target)
      updateAnalysis()
    }
  }
  private def adaptFunction(lambda: SchemeLambdaExp, cmps: Set[Component], target: Int): Unit = {
    // find a fitting k
    var calls = cmps.map(view(_).asInstanceOf[Call[ComponentContext]])
    var k = calls.maxBy(_.ctx.length).ctx.length
    while(calls.size > target && k > 0) {
      k = k - 1
      calls = calls.map(cll => cll.copy(ctx = cll.ctx.take(k)))
    } 
    kPerFn += lambda -> k  // register the new k
    // if lowering context does not work => adapt parent components
    if(calls.size > target) {
      val parentCmps = calls.map(cll => cll.clo._2.asInstanceOf[WrappedEnv[Addr,Component]].data)
      val parentLambda = view(parentCmps.head).asInstanceOf[Call[ComponentContext]].clo._1
      adaptFunction(parentLambda, parentCmps, target)
    }
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
  }
}
