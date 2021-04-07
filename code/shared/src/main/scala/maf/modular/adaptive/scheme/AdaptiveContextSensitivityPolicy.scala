package maf.modular.adaptive.scheme

import maf.modular.adaptive.scheme._
import maf.core.Position._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.Monoid

trait AdaptiveContextSensitivityPolicy extends AdaptiveContextSensitivity {

  // context-sensitivity policy can be configured per closure
  // choice of policies is left open as a parameter; the following needs to be provided:
  // - a starting policy `defaultPolicy` (with high precision)
  // - a `nextPolicy` method, which computes for a given closure the next policy (with lower precision)
  // - a `glbPolicy` method that computes the most precise policy that is less precise than both given policies

  trait ContextSensitivityPolicy {
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext
    def adaptCtx(ctx: ComponentContext): ComponentContext
  }

  val defaultPolicy: ContextSensitivityPolicy
  def nextPolicy(
      clo: lattice.Closure,
      cur: ContextSensitivityPolicy,
      cts: Set[ComponentContext]
    ): ContextSensitivityPolicy
  def glbPolicy(pl1: ContextSensitivityPolicy, pl2: ContextSensitivityPolicy): ContextSensitivityPolicy

  implicit protected val policyMonoid = new Monoid[ContextSensitivityPolicy] {
    def zero = defaultPolicy
    def append(pl1: ContextSensitivityPolicy, pl2: => ContextSensitivityPolicy) = glbPolicy(pl1, pl2)
  }

}

//
// Adaptive KCFA
//

trait AdaptiveKCFA extends AdaptiveContextSensitivityPolicy {

  type ComponentContext = List[Position]

  case object KUnlimited extends ContextSensitivityPolicy {
    override def toString = "k = ∞"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext = call :: getContext(caller)
  }

  case class KCallSites(k: Int) extends ContextSensitivityPolicy {
    override def toString = s"k = $k"
    def adaptCtx(ctx: ComponentContext): ComponentContext = ctx.take(k)
    def allocCtx(
        clo: lattice.Closure,
        args: List[Value],
        call: Position,
        caller: Component
      ): ComponentContext = (call :: getContext(caller)).take(k)
  }

  val defaultPolicy = KUnlimited
  def nextPolicy(
      clo: lattice.Closure,
      cur: ContextSensitivityPolicy,
      cts: Set[ComponentContext]
    ): ContextSensitivityPolicy = cur match {
    case KUnlimited =>
      val highestK = cts.maxBy(_.length).length
      KCallSites(highestK - 1)
    case KCallSites(k) if k > 0 => KCallSites(k - 1)
    case _                      => throw new Exception("Can not lower precision any further!")
  }
  def glbPolicy(pl1: ContextSensitivityPolicy, pl2: ContextSensitivityPolicy) = (pl1, pl2) match {
    case (KUnlimited, _)                  => pl2
    case (_, KUnlimited)                  => pl1
    case (KCallSites(k1), KCallSites(k2)) => KCallSites(Math.min(k1, k2))
  }

  def updateCtx(update: Component => Component)(ctx: ComponentContext) = ctx

  private def getContext(cmp: Component): ComponentContext = view(cmp) match {
    case Main                                   => List.empty
    case cll: Call[ComponentContext] @unchecked => cll.ctx
  }
}
