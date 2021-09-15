package maf.modular.scv
import maf.modular.{GlobalStore, ModAnalysis}
import maf.language.scheme.SchemeExp
import maf.modular.ReturnValue
import maf.modular.ReturnAddr
import maf.modular.scheme.SchemeDomain
import maf.language.ContractScheme.ContractValues._
import maf.core.Identifier

// TODO: put this into its file?
abstract class IsSat[+V]

/** Returned by the solver if there exists a solution for some formula */
case class Sat[+V](solution: Map[Identifier, V]) extends IsSat[V]

/** Returned by the solver if the formula has no solution */
case object Unsat extends IsSat[Nothing]

/** Returns by the solver if it is not known whether the formula has a solution (e.g., when the solver times-out) */
case object Unknown extends IsSat[Nothing]

trait ScvSatSolver[V] {
  def sat(e: List[SchemeExp]): IsSat[V]
  def sat(e: SchemeExp): IsSat[V] = sat(List(e))
  def feasible(e: SchemeExp): Boolean = sat(e) match
      case Sat(_) | Unknown => true
      case _                => false
}

/** Main trait for the soft-contract verification analysis. */
trait ScvModAnalysis extends ModAnalysis[SchemeExp] with GlobalStore[SchemeExp] with ReturnValue[SchemeExp] with SchemeDomain { outer =>
  protected val sat: ScvSatSolver[Value]

  override def intraAnalysis(component: Component): IntraScvAnalysis

  trait IntraScvAnalysis extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra { inner =>
    def writeBlame(blame: Blame): Unit =
      writeAddr(ScvExceptionAddr(component, expr(component).idn), lattice.blame(blame))

  }

  def summary: ScvAnalysisSummary[Value] =
      var returnValues = Map[Any, Value]()
      var blames = Map[Any, Set[Blame]]()

      store.foreach {
        case (ReturnAddr(cmp, _), value)       => returnValues = returnValues.updated(cmp, value)
        case (ScvExceptionAddr(cmp, _), value) => blames = blames.updated(cmp, lattice.getBlames(value))
        case _                                 => ()
      }

      ScvAnalysisSummary(returnValues, blames)

  def getReturnValue(component: Component): Option[Value] =
    summary.getReturnValue(component)
}

/** This class summerizes the results of the analysis of a whole program annotated with contracts */
case class ScvAnalysisSummary[Value](
    returnValues: Map[Any, Value],
    blames: Map[Any, Set[Blame]]):

    def getReturnValue(component: Any): Option[Value] = returnValues.get(component)
