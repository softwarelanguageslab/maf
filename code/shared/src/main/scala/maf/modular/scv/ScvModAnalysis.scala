package maf.modular.scv

import maf.language.symbolic.*
import maf.language.symbolic.lattices.*
import maf.modular.{GlobalStore, ModAnalysis}
import maf.language.scheme.{SchemeExp, SchemeLambdaExp}
import maf.modular.ReturnValue
import maf.modular.ReturnAddr
import maf.modular.scheme.SchemeDomain
import maf.language.ContractScheme.ContractValues._
import maf.core.{Identifier, Identity}
import maf.modular.scheme.modf.SchemeModFComponent
import maf.modular.scheme.modf.BaseSchemeModFSemantics
import maf.core.Address

// TODO: put this into its file?
abstract class IsSat[+V]

/** Returned by the solver if there exists a solution for some formula */
case class Sat[+V](solution: Map[Identifier, V]) extends IsSat[V]

/** Returned by the solver if the formula has no solution */
case object Unsat extends IsSat[Nothing]

/** Returns by the solver if it is not known whether the formula has a solution (e.g., when the solver times-out) */
case object Unknown extends IsSat[Nothing]

trait ScvSatSolver[V] {
    def sat(e: Formula, vars: List[String]): IsSat[V]
    def sat(e: Formula): IsSat[V] = sat(e, List())
    def feasible(e: Formula, vars: List[String]): Boolean = sat(e, vars) match
        case Sat(_) | Unknown => true
        case _                => false
    def feasible(e: Formula): Boolean = feasible(e, List())
}

/** Main trait for the soft-contract verification analysis. */
trait ScvModAnalysis
    extends ModAnalysis[SchemeExp]
    with GlobalStore[SchemeExp]
    with ReturnValue[SchemeExp]
    with SymbolicSchemeDomain
    with ScvBaseSemantics {
    outer =>
    protected val DEBUG: Boolean = true

    protected val sat: ScvSatSolver[Value]

    override def intraAnalysis(component: Component): IntraScvAnalysis

    /** Executes the given function using the contract embedded in the component (if any is available) */
    protected def usingContract[X](cmp: Component)(f: Option[(List[Value], Value, List[SchemeExp], Identity)] => X): X
    protected def usingRangeContract[X](cmp: Component)(f: Option[Value] => X): X

    /* Instead of keeping track of each lexical store cache in the closure itself, we keep a map from closures to store caches */
    protected var lexicalStoCaches: Map[(SchemeLambdaExp, Env), StoreCache] = Map()

    trait FromContext:
        def pathCondition: PathCondition
        def vars: List[String]
        def symbolic: Map[String, Option[SchemeExp]]
        def lexStoCache: Map[Address, SchemeExp]

    object EmptyContext extends FromContext:
        def pathCondition: PathCondition = PathCondition(EmptyFormula)
        def vars: List[String] = List()
        def symbolic: Map[String, Option[SchemeExp]] = Map()
        def lexStoCache: Map[Address, SchemeExp] = Map()

    /** Returns interesting information about the context of the current component */
    protected def fromContext(cmp: Component): FromContext

    trait IntraScvAnalysis extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra with BaseIntraAnalysis { inner =>
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
