package maf.modular.scheme.modf

import maf.core.Position._

// TODO: make allocCtx an abstract method of this trait
trait SchemeModFSensitivity extends BaseSchemeModFSemantics

/* Simplest (and most imprecise): no context-sensitivity */
case object NoContext extends Serializable {
  override def toString: String = "ε" // Mostly for the web visualisation that otherwise prints "undefined".
}

trait SchemeModFNoSensitivity extends SchemeModFSensitivity {
  type ComponentContext = NoContext.type
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext = NoContext
}

/* Full argument sensitivity for ModF */
case class ArgContext(values: List[_]) { //TODO: preserve type information
  override def toString: String = values.mkString(",")
}
trait SchemeModFFullArgumentSensitivity extends SchemeModFSensitivity {
  type ComponentContext = ArgContext
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext = ArgContext(args)
}

/* Call-site sensitivity for ModF */
case class CallSiteContext(calls: List[Position]) {
  override def toString: String = calls.mkString("[", ",", "]")
}
trait SchemeModFKCallSiteSensitivity extends SchemeModFSensitivity {
  val k: Int
  type ComponentContext = CallSiteContext
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ) = context(caller) match {
    case None                           => CallSiteContext(List(call).take(k))
    case Some(CallSiteContext(callers)) => CallSiteContext((call :: callers).take(k))
  }
}
// shorthand for 1-CFA
trait SchemeModFCallSiteSensitivity extends SchemeModFKCallSiteSensitivity {
  override val k = 1
}

/* Call-site x full-argument sensitivity for ModF */
case class ArgCallSiteContext(
    fn: Position,
    call: Position,
    args: List[_]) { //TODO: type information about Value is lost!
  override def toString: String = {
    val argsstr = args.mkString(",")
    s"$call->$fn $argsstr"
  }
}
trait SchemeModFFullArgumentCallSiteSensitivity extends SchemeModFSensitivity {
  type ComponentContext = ArgCallSiteContext
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    ArgCallSiteContext(clo._1.idn.pos, call, args)
}

trait SchemeModFUserGuidedSensitivity1 extends SchemeModFSensitivity {
  type ComponentContext = Any
  def allocCtx(
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ): ComponentContext =
    clo._1.annotation match {
      case None =>
        // println(s"WARNING: Function has no annotation: $nam ($clo), using FA")
        ("No", ())
      case Some(("@sensitivity", "1CS")) =>
        ("1CS", call)
      case Some(("@sensitivity", "2CS")) =>
        context(caller) match {
          case Some(("1CS", call2: Position)) =>
            ("2CS", call :: call2 :: Nil)
          case Some(("2CS", calls: List[Position])) =>
            ("2CS", (call :: calls).take(2))
          case _ =>
            ("2CS", call :: Nil)
        }
      case Some(("@sensitivity", "FA")) =>
        ("FA", args)
      case Some(("@sensitivity", "1A")) =>
        ("1A", args.take(1))
      case Some(("@sensitivity", "2A")) =>
        ("2A", args.drop(1).take(1))
      case Some(("@sensitivity", "No")) =>
        ("No", ())
      case annot =>
        println(s"WARNING: Function has an invalid annotation: (${clo._1.lambdaName}), using no sensitivity instead of: $annot")
        ("No", ())

    }
}
