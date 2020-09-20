package scalaam.modular.contracts

import scalaam.language.contracts.ScLattice.Blame

case class ScAnalysisSummary[Value](
    returnValues: Map[Any, Value],
    blames: Map[Any, Set[Blame]]
) {

  def getReturnValue(component: Any): Option[Value] = returnValues.get(component)
}
