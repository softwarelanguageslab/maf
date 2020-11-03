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

trait AdaptiveContextSensitivity extends AdaptiveSchemeModFSemantics {
  /*
   * configured by some "budget" (= max number of components in the analysis) 
   */
  val budget: Int
  /*
   * contexts are sets of call sites
   * after adaptation, certain call sites can become excluded for a given function
   */
  type ComponentContext = Set[Position]
  var excluded = Map[SchemeLambdaExp, Set[Position]]()
  def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main => Set.empty
    case c: Call[ComponentContext] @unchecked => c.ctx
  }
  def allocCtx(nam: Option[String], clo: lattice.Closure, args: List[Value], call: Position, caller: Component): ComponentContext = {
    val parentContext = getContext(caller)
    if(excluded.getOrElse(clo._1, Set.empty).contains(call)) {
      parentContext
    } else {
      parentContext + call
    }
  }
  def adaptCall(c: Call[ComponentContext]): Call[ComponentContext] =
    c.copy(ctx = adaptCtx(c.clo,c.ctx))
  def adaptCtx(clo: lattice.Closure, ctx: ComponentContext): ComponentContext = 
    ctx -- excluded.getOrElse(clo._1, Set.empty)
  /* 
   * monitoring effects per component 
   */
  var writesPerCmp = Map[Component, Map[AddrLoc, Value]]()
  // "context-insensitive addresses"
  trait AddrLoc  
  case class RetAddrLoc(idn: Identity)  extends AddrLoc
  case class PrmAddrLoc(nam: String)    extends AddrLoc
  case class VarAddrLoc(id: Identifier) extends AddrLoc
  case class PtrAddrLoc(exp: SchemeExp) extends AddrLoc
  private def convertAddr(addr: Addr): Option[AddrLoc] = addr match {
    case PrmAddr(nam)       => Some(PrmAddrLoc(nam))
    case VarAddr(id,_)      => Some(VarAddrLoc(id))
    case PtrAddr(exp,_)     => Some(PtrAddrLoc(exp))
    case ReturnAddr(_,idn)  => Some(RetAddrLoc(idn))
    case _                  => None
  }
  // custom intra-analysis to instrument writes to the store
  override def intraAnalysis(cmp: Component): AdaptiveCSIntra = new AdaptiveCSIntra(cmp)
  class AdaptiveCSIntra(cmp: Component) extends AdaptiveSchemeModFIntra(cmp) {
    override def writeAddr(addr: Addr, value: Value): Boolean = {
      convertAddr(addr).foreach { addrLoc =>
        val previousWrites = writesPerCmp.getOrElse(cmp, Map.empty)
        val updatedWrites = previousWrites + (addrLoc -> value)
        writesPerCmp += cmp -> updatedWrites
      }
      super.writeAddr(addr, value)
    }
  }
  /* data */
  var cmpsPerFn = Map[SchemeLambdaExp, Set[Component]]()
  implicit val valueMonoid: Monoid[Value] = MonoidImplicits.latticeMonoid(lattice)
  /* on new component */ 
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = {
    // bookkeeping: update the function to components mapping
    val lambda = call.clo._1
    val lambdaCmps = cmpsPerFn.getOrElse(lambda, Set.empty)
    val lambdaCmpsUpdated = lambdaCmps + cmp
    cmpsPerFn += lambda -> lambdaCmpsUpdated
  }
  override protected def adaptAnalysis(): Unit = {
    super.adaptAnalysis()
    // check if we need to adapt
    while (visited.size > budget) {
      val (lambda, cmps) = cmpsPerFn.maxBy(_._2.size) // TODO: optimise this operation
      if(cmps.size == 1) { throw new Exception("Budget could not be satified") } // TODO: just increase the budget?
      val target = cmps.size - (visited.size - budget)
      adaptFunction(lambda, cmps, target)
      updateAnalysis()
    }
  }
  private def adaptFunction(lambda: SchemeLambdaExp, cmps: Set[Component], target: Int) = {
    val addrValues = cmps.foldLeft(Map.empty[AddrLoc,Set[Value]]) { (acc,cmp) =>
      writesPerCmp(cmp).foldLeft(acc) { case (acc2, (addr, value)) =>
        acc2 + (addr -> (acc2.getOrElse(addr, Set.empty) + value))
      }
    }.toList.sortBy(_._2.size)
    clusterCmps(lambda, cmps, target, addrValues)
  }
  private def clusterCmps(lambda: SchemeLambdaExp, cmps: Set[Component], target: Int, addrValues: List[(AddrLoc,Set[Value])]): Unit = {
    val clustered = cmps.groupBy { cmp => 
      val writes = writesPerCmp.getOrElse(cmp, Map.empty)
      addrValues.map { case (addr, _) => writes.getOrElse(addr, lattice.bottom) }
    }
    if(clustered.size <= target) {
      // join contexts here
      ???
    } else if (addrValues.nonEmpty) {
      clusterCmps(lambda, cmps, target, addrValues.tail)
    }
  }
  override def updateAnalysisData(update: Component => Component) = {
    super.updateAnalysisData(update)
    this.cmpsPerFn = updateMap(updateSet(update))(cmpsPerFn)
    this.writesPerCmp = updateMap(update, updateMap[AddrLoc,Value](v => updateValue(update)(v)))(writesPerCmp)
  }
}
