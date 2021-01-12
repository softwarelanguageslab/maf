package maf.modular.contracts

import maf.core.{Address, Environment, Identity}
import maf.language.contracts.{ScExp, ScLambda}
import maf.modular.LocalStoreMap

sealed trait ScComponent extends Serializable

/**
  * The main entry point of the program
  */
case object ScMain extends ScComponent

trait Call[Context] extends ScComponent {
  val env: Environment[Address]
  val lambda: ScLambda
  val context: Context

  override def toString: String = lambda.toString
}

object Call {
  def apply[Context](
      env: Environment[Address],
      lambda: ScLambda,
      context: Context
  ): Call[Context] = {
    RegularCall(env, lambda, context)
  }

  def unapply[Context](any: Any): Option[(Environment[Address], ScLambda, Context)] = any match {
    case c: Call[Context] => Some((c.env, c.lambda, c.context))
    case _                => None
  }
}

/**
  * A call to another function
  * @param env the lexical environment of the lambda
  * @param lambda the body of the lambda
  * @tparam Context the context of the call
  */
case class RegularCall[Context](env: Environment[Address], lambda: ScLambda, context: Context)
    extends Call[Context]

case class ContractCall[Context](
    monIdentity: Identity,
    blamedParty: Identity,
    env: Environment[Address],
    lambda: ScLambda,
    context: Context
) extends Call[Context]

case class CallWithStore[Context, Addr, Value](
    call: Call[Context],
    store: LocalStoreMap[Addr, Value]
) extends Call[Context] {
  override val env: Environment[Address] = call.env
  override val lambda: ScLambda          = call.lambda
  override val context: Context          = call.context

  override def toString: String = s"with_store${call.lambda}"
}

trait ScStandardComponents extends ScModSemantics {
  type Component = ScComponent

  def expr(component: Component): ScExp = view(component) match {
    case ScMain             => program
    case Call(_, lambda, _) => lambda
  }

  def view(component: Component): ScComponent = component

  def newComponent(component: Call[ComponentContext]): Component =
    component

  def initialComponent: ScComponent = ScMain
}
