package scalaam.modular.contracts

import scalaam.language.contracts.ScExp

/**
  * Transforms a condition built using basic predicates from the soft contract language
  * into valid assertions for Z3,
  * @param condition
  */
class ScSMTSolver(condition: ScExp) {

  /**
    * Transforms the condition into valid Z3 assertions
    * @return valid Z3 assertions
    */
  private def transform(): String = ???
  def isSat(): Boolean            = ???
}
