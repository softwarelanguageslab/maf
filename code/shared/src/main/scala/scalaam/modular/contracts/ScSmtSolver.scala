package scalaam.modular.contracts

import scalaam.language.contracts.ScExp

trait ScSmtSolver {
  def isSat: Boolean
}
