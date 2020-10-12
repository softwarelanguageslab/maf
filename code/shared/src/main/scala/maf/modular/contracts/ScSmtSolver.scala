package maf.modular.contracts

import maf.language.contracts.ScExp

trait ScSmtSolver {
  def isSat: Boolean
}
