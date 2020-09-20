package scalaam.modular.contracts

import scalaam.core.{Address, Environment, Identifier}
import scalaam.language.contracts.{ScExp, ScLambda}

sealed trait ScComponent

/**
  * The main entry point of the program
  */
case object ScMain extends ScComponent

/**
  * A call to another function
  * @param env the lexical environment of the lambda
  * @param lambda the body of the lambda
  * @tparam Context the context of the call
  */
case class Call[Context](env: Environment[Address], lambda: ScLambda, context: Context)
    extends ScComponent

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
