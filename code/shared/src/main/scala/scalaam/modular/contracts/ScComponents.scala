package scalaam.modular.contracts

import scalaam.core.{Address, Environment, Identifier, Identity}
import scalaam.language.contracts.{ScExp, ScLambda}

sealed trait ScComponent

/**
  * The main entry point of the program
  */
case object ScMain extends ScComponent

trait Call[Context] extends ScComponent {
  val env: Environment[Address]
  val lambda: ScLambda
  val context: Context
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
    case c: Call[Context] => Some(c.env, c.lambda, c.context)
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
