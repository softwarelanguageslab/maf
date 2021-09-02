package maf.modular.scv
import maf.modular.{ModAnalysis, GlobalStore}
import maf.language.scheme.SchemeExp
import maf.modular.ReturnValue
import maf.modular.ReturnAddr
import maf.modular.scheme.SchemeDomain
import maf.language.ContractScheme.ContractValues._

/** 
 *  Main trait for the soft-contract verification analysis. 
 */
trait ScVModAnalysis
  extends ModAnalysis[SchemeExp] 
  with GlobalStore[SchemeExp]
  with ReturnValue[SchemeExp]
  with SchemeDomain { outer =>

  override def intraAnalysis(component: Component): IntraScvAnalysis

  trait IntraScvAnalysis extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra { inner =>
    def writeBlame(blame: Blame): Unit = 
      writeAddr(ScvExceptionAddr(component, expr(component).idn), lattice.blame(blame))

  }

  def summary: ScvAnalysisSummary[Value] = {
    var returnValues = Map[Any, Value]()
    var blames = Map[Any, Set[Blame]]()

    store.foreach {
      case (ReturnAddr(cmp, _), value)    => returnValues = returnValues.updated(cmp, value)
      case (ScvExceptionAddr(cmp, _), value) => blames = blames.updated(cmp, lattice.getBlames(value))
      case _                              => ()
    }

    ScvAnalysisSummary(returnValues, blames)
  }

  def getReturnValue(component: Component): Option[Value] =
    summary.getReturnValue(component)
}


/**
 * This class summerizes the results of the analysis of a whole program annotated with contracts 
 */
case class ScvAnalysisSummary[Value](
    returnValues: Map[Any, Value],
    blames: Map[Any, Set[Blame]]) {

  def getReturnValue(component: Any): Option[Value] = returnValues.get(component)
}
