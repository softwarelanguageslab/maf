package scalaam.modular.contracts

import scalaam.core.{Address, Environment, Identifier}
import scalaam.language.contracts.ScLambda

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
case class Call[Context](env: Environment[Address], lambda: ScLambda) extends ScComponent
